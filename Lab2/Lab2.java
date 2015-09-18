import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.Arrays;
import java.util.ArrayList;

public class Lab1 {

    private final Semaphore[] semaphores = new Semaphore[8];
    //0 - TOP STATION UPPER
    //1 - TOP STATION LOWER
    //2 - BOTTOM STATION UPPER
    //3 - BOTTOM STATION LOWER
    //4 - LEFT BOTTOM TURN
    //5 - MIDDLE LANE
    //6 - RIGHT TOP TURN
    //7 - 4 WAY CROSSING
    private Train top_train;
    private Train bottom_train;
    private int simulation_speed;

    private enum Direction {
        UP,DOWN
    };

    private class Train extends Thread {

        private final int TIME_AT_STATION = 1500; //in milliseconds
        private final int MAXSPEED = 22; //Maximum supported speed

        private int id;
        private int speed; //Train speed
        private TSimInterface tsi;
        private Direction direction; //Up or down the railway
        private boolean is_turning; //state variable for turning

        //Indexes of currently acquired semaphores
        private ArrayList<Integer> acquired_semaphores; 

        private int sensors[][] = {
            {14,3},   /*  0 - UPPER TOP STOP */
            {14,5},   /*  1 - LOWER TOP STOP */
            {14,11},  /*  2 - UPPER BOTTOM STOP */
            {14,13},  /*  3 - LOWER BOTTOM STOP */

            {6,13},   /*  4 - LOWER BOTTOM STATION WAITING POINT */
            {6,11},   /*  5 - UPPER BOTTOM STATION WAITING POINT */

            {1,10},   /*  6 - TURN TO MIDDLE LANE ON THE LEFT */

            {3,9},    /*  7 - BY MIDDLE SWITCH TO THE LEFT */

            {12,9},   /*  8 - TOP MIDDLE LANE STOP TO THE RIGHT*/
            {12,10},  /*  9 - BOTTOM MIDDLE LANE STOP TO THE RIGHT*/

            {18,7},  /*  10 - TURNPOINT BETWEEN TOP STATIONS*/

            {14,7},  /*  11 - WAITING POINT FOR UPPER TOP STATION */

            {6,9},   /*  12 - TOP MIDDLE LANE WAITING POINT TO THE LEFT */
            {6,10},  /*  13 - BOTTOM MIDDLE LANE WAITING POINT TO THE LEFT */

            {17,9},  /*  14 - UPPER BEGINNING OF RIGHT TURN*/

            {14,8}  /*  15 - WAITING POINT FOR LOWER TOP STATION*/
        };

        //Set a switch with a given index, can see coordinates of each sensor in array below
        private void setSwitch(int i, Direction direction) {
            int switches[][] = {
                {3,11}, //0
                {4,9},  //1
                {15,9}, //2
                {17,7}  //3
            };
            try {
                this.tsi.setSwitch(switches[i][0], switches[i][1], direction.ordinal()+1);
            } catch(CommandException e) { e.printStackTrace(); }
        }

        private int speedAtStation() {
            return this.TIME_AT_STATION + 2 * simulation_speed * Math.abs(this.speed);
        }

        //Wrapper that makes use of our list of acquired semaphores
        private void acquireSemaphore(int i) {
            try{
                semaphores[i].acquire();
                acquired_semaphores.add(i);
            } catch(InterruptedException e) { e.printStackTrace(); }
        }

        //Wrapper that makes use of our list of acquired semaphores
        private boolean tryAcquireSemaphore(int i) {
            if(semaphores[i].tryAcquire()){
                acquired_semaphores.add(i);
                return true;
            }
            return false;
        }

        //Wrapper that makes use of our list of acquired semaphores
        private void releaseSemaphore(int i) {
            int index = acquired_semaphores.indexOf(i);
            if(index >= 0) {
                acquired_semaphores.remove(index);
                semaphores[i].release();
            }
        }

        //Stops the train until a semaphore of the given index can be acquired
        private void waitForSemaphore(int i) {
            if(acquired_semaphores.indexOf(i) != -1) {
                //Already have it no need to ask for it again
                return;
            }
            if(!this.tryAcquireSemaphore(i)) {
                int speed = this.speed;
                this.stopTrain();
                this.acquireSemaphore(i);
                this.setSpeed(speed);
            }
        }

        public Train(int id, int train_speed, Direction d) {
            super();
            this.id = id;
            this.tsi = TSimInterface.getInstance();
            this.tsi.setDebug(true);
            this.direction = d;
            this.is_turning = false;
            this.setSpeed(train_speed);
            this.acquired_semaphores = new ArrayList<>();
        }

        //Sets speed of train
        public void setSpeed(int speed) {
            if(speed > MAXSPEED) {
                this.setSpeed(MAXSPEED);
                return;
            }
            try {
                this.tsi.setSpeed(this.id,speed);
                this.speed = speed;
            } catch (CommandException e) { e.printStackTrace(); }
        }

        public void stopTrain() {
            this.setSpeed(0);
        }

        //Makes the train drive in reverse
        public void turnTrain() {
            int speed = this.speed;
            if(this.is_turning) {
                return;
            }
            this.is_turning = true;
            this.stopTrain();
            try {
                this.sleep(this.speedAtStation());
            } catch(InterruptedException e) { e.printStackTrace(); }
            this.setSpeed(-speed);
            this.is_turning = false;
            if(this.direction == Direction.DOWN) {
                this.direction = Direction.UP;
            } else {
                this.direction = Direction.DOWN;
            }
        }

        @Override
        public void run() {
            while(true) {
                try {
                    SensorEvent sensor = tsi.getSensor(this.id);
                    if (sensor.getStatus() == 1) {
                        int pos[] = { sensor.getXpos(), sensor.getYpos() };
                        for(int i=0;i<sensors.length;i++) {
                            if(Arrays.equals(pos,sensors[i])) {
                                if(this.direction == Direction.UP) {
                                    switch(i) { //Index of sensor

                                        case 0:
                                            this.turnTrain();
                                        break;

                                        case 1:
                                            this.turnTrain();
                                        break;

                                        case 2:
                                        break;

                                        case 3:
                                        break;

                                        case 4:
                                            this.waitForSemaphore(4);
                                            this.setSwitch(0,Direction.DOWN);
                                        break;

                                        case 5:
                                            this.waitForSemaphore(4);
                                            this.setSwitch(0,Direction.UP);
                                        break;

                                        case 6:
                                            this.releaseSemaphore(2);
                                        break;

                                        case 7:
                                            if(this.tryAcquireSemaphore(5)) {
                                                this.setSwitch(1,Direction.UP);
                                            } else {
                                                this.setSwitch(1,Direction.DOWN);
                                            }
                                        break;

                                        case 8:
                                            this.waitForSemaphore(6);
                                            this.setSwitch(2,Direction.DOWN);
                                        break;

                                        case 9:
                                            this.waitForSemaphore(6);
                                            this.setSwitch(2,Direction.UP);
                                        break;

                                        case 10:
                                            if(this.tryAcquireSemaphore(1)) {
                                                this.setSwitch(3,Direction.UP);
                                            } else {
                                                this.setSwitch(3,Direction.DOWN);
                                            }
                                        break;

                                        case 11:
                                            this.releaseSemaphore(6);
                                            this.waitForSemaphore(7);
                                        break;

                                        case 12:
                                            this.releaseSemaphore(4);
                                        break;

                                        case 13:
                                            this.releaseSemaphore(4);
                                        break;

                                        case 14:
                                            this.releaseSemaphore(5);
                                        break;

                                        case 15:
                                            this.releaseSemaphore(6);
                                            this.waitForSemaphore(7);
                                        break;

                                    }
                                }
                                else if(this.direction == Direction.DOWN) {
                                    switch(i) { //Index of sensor

                                        case 0:
                                            this.waitForSemaphore(7);
                                        break;

                                        case 1:
                                            this.waitForSemaphore(7);
                                        break;

                                        case 2:
                                            this.turnTrain();
                                        break;

                                        case 3:
                                            this.turnTrain();
                                        break;

                                        case 4:
                                            this.releaseSemaphore(4);
                                        break;

                                        case 5:
                                            this.releaseSemaphore(4);
                                        break;

                                        case 6:
                                            if(this.tryAcquireSemaphore(2)) {
                                                this.setSwitch(0,Direction.UP);
                                            } else {
                                                this.setSwitch(0,Direction.DOWN);
                                            }
                                        break;

                                        case 7:
                                            this.releaseSemaphore(5);
                                        break;

                                        case 8:
                                            this.releaseSemaphore(6);
                                            this.setSwitch(2,Direction.DOWN);
                                        break;

                                        case 9:
                                            this.releaseSemaphore(6);
                                            this.setSwitch(2,Direction.UP);
                                        break;

                                        case 10:
                                            this.releaseSemaphore(1);
                                        break;

                                        case 11:
                                            this.releaseSemaphore(7);
                                            this.waitForSemaphore(6);
                                            this.setSwitch(3,Direction.DOWN);
                                        break;

                                        case 12:
                                            this.waitForSemaphore(4);
                                            this.setSwitch(1,Direction.UP);
                                        break;

                                        case 13:
                                            this.waitForSemaphore(4);
                                            this.setSwitch(1,Direction.DOWN);
                                        break;

                                        case 14:
                                            if(this.tryAcquireSemaphore(5)) {
                                                this.setSwitch(2,Direction.DOWN);
                                            } else {
                                                this.setSwitch(2,Direction.UP);
                                            }
                                        break;

                                        case 15:
                                            this.releaseSemaphore(7);
                                            this.waitForSemaphore(6);
                                            this.setSwitch(3,Direction.UP);
                                        break;

                                    }
                                }
                            }
                        }
                    }
                }
                catch(CommandException e)     { e.printStackTrace(); }
                catch(InterruptedException e) { e.printStackTrace() ;}
            }
        }
    }

    public Lab1(String[] args) {
        //DEFAULTS
        int simulation_speed = 20;
        int bottom_train_speed = 5;
        int top_train_speed = bottom_train_speed;

        //Use the given arguments if provided
        try {
            top_train_speed = Integer.parseInt(args[0]);
        } catch(ArrayIndexOutOfBoundsException e){}

        try {
            bottom_train_speed = Integer.parseInt(args[1]);
        } catch(ArrayIndexOutOfBoundsException e){}

        try {
            simulation_speed = Integer.parseInt(args[2]);
        } catch(ArrayIndexOutOfBoundsException e){}

        for (int i = 0; i < semaphores.length; i++) {
            semaphores[i] = new Semaphore(1,true);
        }

        this.top_train = new Train(1, top_train_speed, Direction.DOWN);
        this.bottom_train = new Train(2, bottom_train_speed, Direction.UP);

        this.top_train.start();
        this.bottom_train.start();
    }

    public static void main(String[] args) {
        new Lab1(args);
    }

}
