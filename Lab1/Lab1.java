import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.Arrays;

public class Lab1 {

    private final Semaphore[] semaphores = new Semaphore[1];
    private Train top_train;
    private Train bottom_train;

    private class Train extends Thread {
        private final int TIME_AT_STATION = 1;
        private final int MAXSPEED = 19;

        private int id;
        private int speed;
        private int sim_speed;
        private TSimInterface tsi;

        private int sensors[][] = {
            {2,9}
        };

        private int speedAtStation() {
            return this.TIME_AT_STATION + 2 * this.sim_speed * Math.abs(this.speed);
        }

        public Train(int id, int train_speed, int simulation_speed) {
            super();
            this.id = id;
            this.tsi = TSimInterface.getInstance();
            this.sim_speed = simulation_speed;
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
            } catch (CommandException e) {}
        }

        public void stopTrain() {
            this.setSpeed(0);
        }

        @Override
        public void run() {
            while(true) {
                try {
                    SensorEvent sensor = tsi.getSensor(this.id);
                    if (sensor.getStatus() == 1) {
                        int pos[] = {sensor.getXpos(), sensor.getYpos()};
                        for(int i=0;i<sensors.length;i++) {
                            if(Arrays.equals(pos,sensors[i])) {

                                switch(i) {
                                    case 0:
                                    System.exit(1);
                                    break;
                                }

                                break;
                            }
                        }
                    }
                }
                catch(CommandException e) {}
                catch(InterruptedException e) {}
            }
        }

    }

    public Lab1(String[] args) {
        //DEFAULTS
        int top_train_speed = 0;
        int bottom_train_speed = 0;
        int simulation_speed = 100;

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

        this.top_train = new Train(1, top_train_speed, simulation_speed);
        this.bottom_train = new Train(2, bottom_train_speed, simulation_speed);

        this.top_train.start();
        this.bottom_train.start();
    }

    public static void main(String[] args) {
        new Lab1(args);
    }

}
