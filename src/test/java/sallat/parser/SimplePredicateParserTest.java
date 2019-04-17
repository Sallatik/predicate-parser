package sallat.parser;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class SimplePredicateParserTest {

    private PredicateParser<Integer> parser;

    @BeforeAll
    private void init() {

        Map<String, Predicate<Integer>> predicateMap = new HashMap<>();

        predicateMap.put("even", n -> n % 2 == 0);
        predicateMap.put("positive", n -> n > 0);
        predicateMap.put("negative", n -> n < 0);
        predicateMap.put("zero", n -> n == 0);
        predicateMap.put("ten", n -> n == 10);

        Map<String, Operators> operatorsMap = new HashMap<>();

        operatorsMap.put("not", Operators.NOT);
        operatorsMap.put("or", Operators.OR);
        operatorsMap.put("and", Operators.AND);

        parser = SimplePredicateParser.<Integer>builder()
                .setPredicateMap(predicateMap)
                .setOperatorMap(operatorsMap)
                .setCasePolicy(CasePolicy.TO_LOWER_CASE)
                .build();
    }

    @Test
    void single() {

        Predicate<Integer> predicate = parser.parse("even");

        assertTrue(predicate.test(2));
        assertTrue(predicate.test(-4));
        assertFalse(predicate.test(3));
        assertFalse(predicate.test(-1));
    }

    @Test
    void not() {

        Predicate<Integer> predicate =
                parser.parse("not negative");

        assertTrue(predicate.test(0));
        assertTrue(predicate.test(32));
        assertFalse(predicate.test(-12));
    }

    @Test
    void or() {

        Predicate<Integer> predicate =
                parser.parse("negative or ten");

        assertTrue(predicate.test(-32));
        assertTrue(predicate.test(10));

        assertFalse(predicate.test(32));
        assertFalse(predicate.test(0));
    }

    @Test
    void and() {

        Predicate<Integer> predicate =
                parser.parse("positive and even");

        assertTrue(predicate.test(6));
        assertTrue(predicate.test(2));

        assertFalse(predicate.test(0));
        assertFalse(predicate.test(-2));
        assertFalse(predicate.test(3));
        assertFalse(predicate.test(-5));
    }

    @Test
    void twoOperators() {

        Predicate<Integer> predicate =
                parser.parse("positive and not ten");

        assertTrue(predicate.test(3));
        assertTrue(predicate.test(52));

        assertFalse(predicate.test(0));
        assertFalse(predicate.test(-23));
        assertFalse(predicate.test(10));
    }

    @Test
    void manyOperators() {

        Predicate<Integer> predicate =
                parser.parse("zero or negative and not even");

        assertTrue(predicate.test(0));
        assertTrue(predicate.test(-3));

        assertFalse(predicate.test(4));
        assertFalse(predicate.test(5));
        assertFalse(predicate.test(-4));
    }

    @Test
    void parenthesis() {

        Predicate<Integer> predicate =
                parser.parse("(ten or negative) and even");

        assertTrue(predicate.test(10));
        assertTrue(predicate.test(-22));

        assertFalse(predicate.test(5));
        assertFalse(predicate.test(6));
    }
}