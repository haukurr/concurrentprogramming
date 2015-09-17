import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.Arrays;
import java.util.Deque;
import java.util.ArrayDeque;

public class Lab1 {

    private final Semaphore[] semaphores = new Semaphore[9];
    //0 - TOP STATION UPPER
    //1 - TOP STATION LOWER
    //2 - BOTTOM STATION UPPER
    //3 - BOTTOM STATION LOWER
    //4 - FIRST BOTTOM TURN
    //5 - MIDDLE LANE
    //6 - MIDDLE TURN
    //7 - 4 WAY CROSSING
    private Train top_train;
    private Train bottom_train;
    private int simulation_speed;

    private enum Direction {
        UP,DOWN
    };

    private class Train extends Thread {

        private final int TIME_AT_STATION = 1500;
        private final int MAXSPEED = 19;

        private int id;
        private int speed;
        private TSimInterface tsi;
        private Direction direction;
        private boolean is_turning;
        Deque<Semaphore> stack = new ArrayDeque<Semaphore>();

        private int sensors[][] = {
            {14,3},  /*  0 - UPPER TOP STOP */
            {14,5},  /*  1 - LOWER TOP STOP */
            {14,11}, /*  2 - UPPER BOTTOM STOP */
            {14,13},  /*  3 - LOWER BOTTOM STOP */

            {4,13},  /*  4 - LOWER BOTTOM STATION WAITING POINT */
            {4,11},  /*  5 - UPPER BOTTOM STATION WAITING POINT */

            {2,11},  /*  6 - FIRST BOTTOM TURN */
            {3,9},  /*  7 - BY MIDDLE SWITCH TO THE LEFT */

            {12,9},  /*  8 - TOP MIDDLE LANE STOP */
            {12,10},  /*  9 - BOTTOM MIDDLE LANE STOP */

            {18,7},  /*  10 - TURNPOINT BETWEEN TOP STATIONS*/
            {14,7},  /*  11 - MIDDLE TURN WAITING POINT*/

            {5,9},  /*  12 - MIDDLE TOP LANE WAITING POINT*/
            {5,10},  /*  13 - MIDDLE BOTTOM LANE WAITING POINT*/

            {17,9},  /*  14 - UPPER BEGINNING OF RIGHT TURN*/
            {14,8}  /*  15 - LOWER BEGINNING OF RIGHT TURN*/
        };

        private void setSwitch(int i, Direction direction) {
            int switches[][] = {
                {3,11},
                {4,9},
                {15,9},
                {17,7}
            };
            try {
                this.tsi.setSwitch(switches[i][0], switches[i][1], direction.ordinal()+1);
            } catch(CommandException e) { e.printStackTrace(); }
        }

        private int speedAtStation() {
            return this.TIME_AT_STATION + 2 * simulation_speed * Math.abs(this.speed);
        }

        private void releaseSemaphore() {
            stack.pop().release();
        }

        private void waitForSemaphore(int i) {
            if(!semaphores[i].tryAcquire()) {
                int speed = this.speed;
                this.stopTrain();
                try {
                    semaphores[i].acquire();
                } catch(InterruptedException e) { e.printStackTrace(); }
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
        }

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
                                    switch(i) {

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
                                            if(semaphores[2].availablePermits() == 0) {
                                                semaphores[2].release();
                                            }
                                        break;

                                        case 7:
                                            if(semaphores[5].tryAcquire()) {
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
                                            if(semaphores[1].tryAcquire()) {
                                                this.setSwitch(3,Direction.UP);
                                            } else {
                                                this.setSwitch(3,Direction.DOWN);
                                            }
                                        break;

                                        case 11:
                                            if(semaphores[6].availablePermits() == 0) {
                                                semaphores[6].release();
                                            }
                                        break;

                                        case 12:
                                            if(semaphores[4].availablePermits() == 0) {
                                                semaphores[4].release();
                                            }
                                        break;

                                        case 13:
                                            if(semaphores[4].availablePermits() == 0) {
                                                semaphores[4].release();
                                            }
                                        break;

                                        case 14:
                                            if(semaphores[5].availablePermits() == 0) {
                                                semaphores[5].release();
                                            }
                                        break;

                                        case 15:
                                            if(semaphores[6].availablePermits() == 0) {
                                                semaphores[6].release();
                                            }
                                        break;

                                    }
                                }
                                else if(this.direction == Direction.DOWN) {
                                    switch(i) {

                                        case 0:
                                        break;

                                        case 1:
                                        break;

                                        case 2:
                                            this.turnTrain();
                                        break;

                                        case 3:
                                            this.turnTrain();
                                        break;

                                        case 4:
                                            if(semaphores[4].availablePermits() == 0) {
                                                semaphores[4].release();
                                            }
                                        break;

                                        case 5:
                                            if(semaphores[4].availablePermits() == 0) {
                                                semaphores[4].release();
                                            }
                                        break;

                                        case 6:
                                            if(semaphores[2].tryAcquire()) {
                                                this.setSwitch(0,Direction.UP);
                                            } else {
                                                this.setSwitch(0,Direction.DOWN);
                                            }
                                        break;

                                        case 7:
                                            if(semaphores[5].availablePermits() == 0) {
                                                semaphores[5].release();
                                            }
                                        break;

                                        case 8:
                                            if(semaphores[6].availablePermits() == 0) {
                                                semaphores[6].release();
                                            }
                                            this.setSwitch(2,Direction.DOWN);
                                        break;

                                        case 9:
                                            if(semaphores[6].availablePermits() == 0) {
                                                semaphores[6].release();
                                            }
                                            this.setSwitch(2,Direction.DOWN);
                                        break;

                                        case 10:
                                            if(semaphores[1].availablePermits() == 0) {
                                                semaphores[1].release();
                                            }
                                        break;

                                        case 11:
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
                                            if(semaphores[5].tryAcquire()) {
                                                this.setSwitch(2,Direction.DOWN);
                                            } else {
                                                this.setSwitch(2,Direction.UP);
                                            }
                                        break;

                                        case 15:
                                            semaphores[1].release();
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
