package sallat.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Simple minimal implementation of a <code>PredicateParser</code>.<br>
 * This implementation uses mappers, supplied by the user trough the builder.<br>
 * @param <T> type of arguments for <code>Predicate</code>
 */
public class SimplePredicateParser<T> implements PredicateParser<T> {

    private Function<String, Operators> stringToOperator;
    private Function<String, Predicate<T>> stringToPredicate;

    private CasePolicy casePolicy;

    private Object resolve(String token) {

        try {

            Operators operator = stringToOperator.apply(token);

            if (operator != null)
                return operator;

            Predicate<T> predicate = stringToPredicate.apply(token);

            if (predicate != null)
                return predicate;

        } catch (Exception e) {
            throw new RuntimeException("Unknown token: " + token, e);
        }

        throw new RuntimeException("Unknown token: " + token);
    }

    @Override
    public Predicate<T> parse(String expression) {

        expression = prepare(expression);
        List<Object> tokens = getTokens(expression);
        return process(tokens);
    }

    private String prepare(String expression) {

        expression = expression.trim();

        switch (casePolicy) {

            case TO_LOWER_CASE:
                return expression.toLowerCase();
            case TO_UPPER_CASE:
                return expression.toUpperCase();
            default:
                return expression;
        }
    }

    private List<Object> getTokens(String expression) {

        List<Object> tokens = new ArrayList<>();

        int i = 0;
        while(i < expression.length()) {

            if (expression.charAt(i) == ' ') {

                i++;

            } else if (expression.charAt(i) == '(') {

                int parenthesis = getMatchingParenthesis(expression, i);
                tokens.add(parse(expression.substring(i + 1, parenthesis)));
                i = parenthesis + 1;

            } else {

                int word = expression.indexOf(' ', i);

                if (word == -1)
                    word = expression.length();

                tokens.add(resolve(expression.substring(i, word)));

                i = word + 1;
            }
        }

        return tokens;
    }

    private int getMatchingParenthesis(String str, int index) {

        int count = 0;

        for (int i = index; i < str.length(); i++) {

            if (str.charAt(i) == '(')
                count++;

            else if (str.charAt(i) == ')')
                count--;

            if (count == 0)
                return i;
        }

        throw new RuntimeException("Unclosed parenthesis on index " + index);
    }

    private Predicate<T> process(List<Object> tokens) {

        for (int i = 0; i < tokens.size(); i++)
            if (Operators.NOT.equals(tokens.get(i))) {

                if (i + 1 >= tokens.size())
                    throw new RuntimeException("Operand expected");

                Object operand = tokens.get(i + 1);
                if (! (operand instanceof Predicate))
                    throw new RuntimeException("Illegal operand for NOT: " + operand);

                Predicate<T> result = ((Predicate<T>) operand).negate();

                tokens.remove(i);
                tokens.remove(i);
                tokens.add(i, result);
            }

        for (int i = 0; i < tokens.size(); i++)
            if (Operators.AND.equals(tokens.get(i))) {

                if (i - 1 < 0 || i + 1 >= tokens.size())
                    throw new RuntimeException("Operand expected");

                Object operand1 = tokens.get(i - 1);
                Object operand2 = tokens.get(i + 1);

                if (!(operand1 instanceof Predicate && operand2 instanceof Predicate))
                    throw new RuntimeException("Illegal operands for AND: " + operand1 + ", " + operand2);

                Predicate<T> result = ((Predicate<T>) operand1).and((Predicate<T>) operand2);

                tokens.remove(i - 1);
                tokens.remove(i - 1);
                tokens.remove(i - 1);

                tokens.add(i - 1, result);
                i--;
            }

        for (int i = 0; i < tokens.size(); i++)
            if (Operators.OR.equals(tokens.get(i))) {
                if (i - 1 < 0 || i + 1 >= tokens.size())
                    throw new RuntimeException("Operand expected");

                Object operand1 = tokens.get(i - 1);
                Object operand2 = tokens.get(i + 1);

                if (!(operand1 instanceof Predicate && operand2 instanceof Predicate))
                    throw new RuntimeException("Illegal operands for OR: " + operand1 + ", " + operand2);

                Predicate<T> result = ((Predicate<T>) operand1).or((Predicate<T>) operand2);

                tokens.remove(i - 1);
                tokens.remove(i - 1);
                tokens.remove(i - 1);

                tokens.add(i - 1, result);
                i--;
            }

        if (tokens.size() == 1)
            return (Predicate<T>) tokens.get(0);
        else
            throw new RuntimeException("Unexpected tokens: " + tokens);
    }

    SimplePredicateParser(Function<String, Operators> stringToOperator, Function<String, Predicate<T>> stringToPredicate, CasePolicy casePolicy) {
        this.stringToOperator = stringToOperator;
        this.stringToPredicate = stringToPredicate;
        this.casePolicy = casePolicy;
    }

    /**
     * Creates new builder.
     * @param <T> generic type argument for the instance to be constructed.
     * @return <code>SimplePredicateBuilder</code> instance.
     */
    public static <T> SimplePredicateParserBuilder<T> builder() {
        return new SimplePredicateParserBuilder<>();
    }
}
