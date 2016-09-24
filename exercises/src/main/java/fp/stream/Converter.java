package fp.stream;

import java.math.BigDecimal;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Created by apple on 9/24/16.
 */
@FunctionalInterface
public interface Converter<F,T> {
    T convert(F from);
}

class Something {
    String startWith(String s) {
        return String.valueOf( s.charAt(0) );
    }
}

class ConverterTest {

    public static void main(String[] args) {

//        Converter<String,Integer> converter = (from) -> Integer.valueOf(from);
        Converter<String,Integer> converter = Integer::valueOf;
        System.out.println(converter.convert("233"));

        Converter<String,String> starter = new Something()::startWith;

        System.out.println(starter.convert("Scala"));

        Function<String,Integer> toInteger = (str) -> Integer.valueOf(str);
        Function<String,Double> toDouble = Double::valueOf;

        System.out.println( toInteger.apply("11"));
        System.out.println( toDouble.apply("23.233"));

        BiFunction<BigDecimal, BigDecimal, Double> adder = (m,n) -> Double.valueOf( m.multiply(n).doubleValue() );

        System.out.println(adder.apply( BigDecimal.valueOf(10), BigDecimal.valueOf(33.98)).doubleValue());
    }
}