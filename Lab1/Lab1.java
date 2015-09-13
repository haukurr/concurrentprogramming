import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.Arrays;

public class Lab1 {

    private final Semaphore[] semaphores = new Semaphore[9];
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

        private int sensors[][] = {
            {14,3}, //UPPER TOP STOP
            {14,5}, //LOWER TOP STOP
            {14,11}, //UPPER BOTTOM STOP
            {14,13} //LOWER BOTTOM STOP
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
            } catch(CommandException e) { System.exit(1); }
        }

        private int speedAtStation() {
            return this.TIME_AT_STATION + 2 * simulation_speed * Math.abs(this.speed);
        }

        public Train(int id, int train_speed, Direction d) {
            super();
            this.id = id;
            this.tsi = TSimInterface.getInstance();
            this.direction = d;
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
            } catch (CommandException e) { System.exit(1); }
        }

        public void stopTrain() {
            this.setSpeed(0);
        }

        public void turnTrain() {
            this.setSpeed(-this.speed);
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
                                        case 0: case 1:
                                        this.turnTrain();
                                        break;
                                    }
                                }
                                if(this.direction == Direction.DOWN) {
                                    switch(i) {
                                        case 2: case 3:
                                        this.turnTrain();
                                        break;
                                    }
                                } else
                                break;
                            }
                        }
                    }
                }
                catch(CommandException e)     { System.exit(1); }
                catch(InterruptedException e) { System.exit(1); }
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

        //this.top_train = new Train(1, top_train_speed, Direction.DOWN);
        this.bottom_train = new Train(2, bottom_train_speed, Direction.UP);

        //this.top_train.start();
        this.bottom_train.start();
    }

    public static void main(String[] args) {
        new Lab1(args);
    }

}
