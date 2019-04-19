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

    private PredicateParser<String> parser;

    @BeforeAll
    void init() {

        Map<String, Operators> operatorsMap = new HashMap<>();
        operatorsMap.put("not", Operators.NOT);
        operatorsMap.put("and", Operators.AND);
        operatorsMap.put("xor", Operators.XOR);
        operatorsMap.put("or", Operators.OR);

        parser = SimplePredicateParser.<String>builder()
                .setCasePolicy(CasePolicy.TO_LOWER_CASE)
                .setOperatorMap(operatorsMap)
                .setPredicateMapping(key -> string -> string.contains(key))
                .build();
    }

    @Test
    void singleFilterTest() {

        Predicate<String> predicate = parser.parse("cat");

        assertTrue(predicate.test("cat"));
        assertFalse(predicate.test("dog"));
    }

    @Test
    void orTest() {

        Predicate<String> predicate = parser.parse("cat or dog");

        assertTrue(predicate.test("cat"));
        assertTrue(predicate.test("dog"));
        assertTrue(predicate.test("cat, dog"));

        assertFalse(predicate.test("pigeon"));
    }

    @Test
    void andTest() {

        Predicate<String> predicate = parser.parse("cat and dog");

        assertTrue(predicate.test("cat, dog"));

        assertFalse(predicate.test("cat"));
        assertFalse(predicate.test("dog"));
        assertFalse(predicate.test("pigeon"));
    }

    @Test
    void notTest() {

        Predicate<String> predicate = parser.parse("not cat");

        assertTrue(predicate.test("dog"));

        assertFalse(predicate.test("cat"));
    }

    @Test
    void multipleOperatorsTest() {

        Predicate<String> predicate = parser.parse("cat and dog and hamster");

        assertTrue(predicate.test("cat, dog, hamster"));

        assertFalse(predicate.test("dog"));
        assertFalse(predicate.test("cat"));
        assertFalse(predicate.test("hamster"));
        assertFalse(predicate.test("dog, cat"));
        assertFalse(predicate.test("cat, hamster"));
        assertFalse(predicate.test("dog, hamster"));
    }

    @Test
    void percedenceTest() {

        Predicate<String> predicate = parser.parse("cat or dog and hamster");

        assertTrue(predicate.test("cat"));
        assertTrue(predicate.test("dog, hamster"));
        assertTrue(predicate.test("cat, dog, hamster"));
        assertTrue(predicate.test("cat, hamster"));

        assertFalse(predicate.test("dog"));
        assertFalse(predicate.test("hamster"));
    }

    @Test
    void parenthesesTest() {

        Predicate<String> predicate = parser.parse("(cat or dog) and hamster");

        assertTrue(predicate.test("cat, hamster"));
        assertTrue(predicate.test("dog, hamster"));
        assertTrue(predicate.test("cat, dog, hamster"));

        assertFalse(predicate.test("cat"));
        assertFalse(predicate.test("dog"));
        assertFalse(predicate.test("hamster"));
        assertFalse(predicate.test("cat, dog"));
    }

    @Test
    void noOperandsTest() {

        assertThrows(RuntimeException.class, () -> parser.parse("and or not"));
    }

    @Test
    void unclosedParentheses() {

        assertThrows(RuntimeException.class, () -> parser.parse("(cat and dog"));
    }

    @Test
    void oneMoreTest() {

        parser.parse("not(cat or dog)");
    }

    @Test
    void xorTest() {

        Predicate<String> predicate = parser.parse("cat xor dog");

        assertTrue(predicate.test("cat"));
        assertTrue(predicate.test("dog"));

        assertFalse(predicate.test("dog, cat"));
        assertFalse(predicate.test("hamster"));
    }
}