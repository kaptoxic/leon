package leon.codegen.runtime;

public class Ticker {
    private int ticksLeft;

    public Ticker(int maxTicks) {
        this.ticksLeft = maxTicks;
    }

    public void tick() throws LeonCodeGenEvaluationException {
    	System.out.println("Ticks: " + ticksLeft);
    	System.err.println("Ticks: " + ticksLeft);
        if(ticksLeft <= 0) {
            throw new LeonCodeGenEvaluationException("Maximum number of evaluation steps reached.");
        }
        ticksLeft--;
    }
    
    public void printTicks() {
    	System.out.println("Ticks: " + ticksLeft);
    }
}
