import TSim.*;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.concurrent.locks.*;

public class Lab2 {

    private class TrackMonitor {
        private final Lock lock = new ReentrantLock();
        private final Condition cond = lock.newCondition();
        private boolean free;

        public void enter() {
            try {
                this.lock.lock();
                if(!this.free) this.cond.await();
                this.free = false;
                this.lock.unlock();
            } catch(InterruptedException e) { e.printStackTrace(); }
        }

        public void leave() {
            this.lock.lock();
            this.free = true;
            this.cond.signal();
            this.lock.unlock();
        }

        public boolean tryLock() {
            this.lock.lock();
            if(!this.free) return false;
            this.enter();
            this.lock.unlock();
            return true;
        }

        public TrackMonitor() {
            this.free = true;
        }

    }

    private final TrackMonitor[] monitors = new TrackMonitor[8];
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

        //Stops the train until a monitor of the given index can be acquired
        private void waitForMonitor(int i) {
            TrackMonitor monitor = monitors[i];
            if(!monitor.tryLock()) {
                int speed = this.speed;
                this.stopTrain();
                monitor.enter();
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
                                            this.waitForMonitor(4);
                                            this.setSwitch(0,Direction.DOWN);
                                        break;

                                        case 5:
                                            this.waitForMonitor(4);
                                            this.setSwitch(0,Direction.UP);
                                        break;

                                        case 6:
                                            monitors[2].leave();
                                        break;

                                        case 7:
                                            if(monitors[5].tryLock()) {
                                                this.setSwitch(1,Direction.UP);
                                            } else {
                                                this.setSwitch(1,Direction.DOWN);
                                            }
                                        break;

                                        case 8:
                                            this.waitForMonitor(6);
                                            this.setSwitch(2,Direction.DOWN);
                                        break;

                                        case 9:
                                            this.waitForMonitor(6);
                                            this.setSwitch(2,Direction.UP);
                                        break;

                                        case 10:
                                            if(monitors[1].tryLock()) {
                                                this.setSwitch(3,Direction.UP);
                                            } else {
                                                this.setSwitch(3,Direction.DOWN);
                                            }
                                        break;

                                        case 11:
                                            monitors[6].leave();
                                            this.waitForMonitor(7);
                                        break;

                                        case 12:
                                            monitors[4].leave();
                                        break;

                                        case 13:
                                            monitors[4].leave();
                                        break;

                                        case 14:
                                            monitors[5].leave();
                                        break;

                                        case 15:
                                            monitors[6].leave();
                                            this.waitForMonitor(7);
                                        break;

                                    }
                                }
                                else if(this.direction == Direction.DOWN) {
                                    switch(i) { //Index of sensor

                                        case 0:
                                            this.waitForMonitor(7);
                                        break;

                                        case 1:
                                            this.waitForMonitor(7);
                                        break;

                                        case 2:
                                            this.turnTrain();
                                        break;

                                        case 3:
                                            this.turnTrain();
                                        break;

                                        case 4:
                                            monitors[4].leave();
                                        break;

                                        case 5:
                                            monitors[4].leave();
                                        break;

                                        case 6:
                                            if(monitors[2].tryLock()) {
                                                this.setSwitch(0,Direction.UP);
                                            } else {
                                                this.setSwitch(0,Direction.DOWN);
                                            }
                                        break;

                                        case 7:
                                            monitors[5].leave();
                                        break;

                                        case 8:
                                            monitors[6].leave();
                                            this.setSwitch(2,Direction.DOWN);
                                        break;

                                        case 9:
                                            monitors[6].leave();
                                            this.setSwitch(2,Direction.UP);
                                        break;

                                        case 10:
                                            monitors[1].leave();
                                        break;

                                        case 11:
                                            monitors[7].leave();
                                            this.waitForMonitor(6);
                                            this.setSwitch(3,Direction.DOWN);
                                        break;

                                        case 12:
                                            this.waitForMonitor(4);
                                            this.setSwitch(1,Direction.UP);
                                        break;

                                        case 13:
                                            this.waitForMonitor(4);
                                            this.setSwitch(1,Direction.DOWN);
                                        break;

                                        case 14:
                                            if(monitors[5].tryLock()) {
                                                this.setSwitch(2,Direction.DOWN);
                                            } else {
                                                this.setSwitch(2,Direction.UP);
                                            }
                                        break;

                                        case 15:
                                            monitors[7].leave();
                                            this.waitForMonitor(6);
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

    public Lab2(String[] args) {
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

        for (int i = 0; i < monitors.length; i++) {
            monitors[i] = new TrackMonitor();
        }

        this.top_train = new Train(1, top_train_speed, Direction.DOWN);
        this.bottom_train = new Train(2, bottom_train_speed, Direction.UP);

        this.top_train.start();
        this.bottom_train.start();
    }

    public static void main(String[] args) {
        new Lab2(args);
    }

}
