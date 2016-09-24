package fp.stream;

/**
 * Created by apple on 9/24/16.
 */

@FunctionalInterface
public interface Formula {
    double calculate(int num);

    default double sqrt(int num) {
        return Math.sqrt(num);
    }
}

class FormulaImpl implements Formula {

    @Override
    public double calculate(int num) {
        return sqrt(num);
    }

    public static void main(String[] args){
        Formula formula = new FormulaImpl();

        System.out.println( formula.calculate(6) );
    }
}



