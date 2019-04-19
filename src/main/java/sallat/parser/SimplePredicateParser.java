package sallat.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import static sallat.parser.Operators.*;
import static sallat.parser.Util.*;

/**
 * Simple minimal implementation of a <code>PredicateParser</code>.<br>
 * This implementation uses mappers, supplied by the user trough the builder.<br>
 * @param <T> type of arguments for <code>Predicate</code>
 */
public class SimplePredicateParser<T> implements PredicateParser<T> {

    private Function<String, Operators> operatorMapping;
    private Function<String, Predicate<T>> predicateMapping;

    private CasePolicy casePolicy;

    @Override
    public Predicate<T> parse(String expression) {

        expression = prepare(expression);
        List<Token<T>> tokens = toTokenList(expression);
        return process(tokens);
    }

    /*
    Resolve the String token to either Operators constant or Predicate, returning
    the result as Object.
    */
    private Token<T> toToken(String token) {

        try {

            Operators operator = operatorMapping.apply(token);

            if (operator != null)
                return new Token<>(operator);

            Predicate<T> predicate = predicateMapping.apply(token);

            if (predicate != null)
                return new Token<>(predicate);

        } catch (Exception e) {
            throw new RuntimeException("Unknown token: " + token, e);
        }

        throw new RuntimeException("Unknown token: " + token);
    }

    /*
    Prepare input String before processing by trimming
    and fixing the case according to case policy.
    */
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

    private List<Token<T>> toTokenList(String expression) {

        List<Token<T>> tokens = new ArrayList<>();

        int i = 0;
        while(i < expression.length()) {

            char ch = expression.charAt(i);

            if (Character.isWhitespace(ch))
                // just go to the next character
                i++;

            else if (ch == '(') {

                int matchingParenthesis = findMatchingParenthesis(expression, i);
                String insideParentheses = expression.substring(i + 1, matchingParenthesis);
                // recursively parse expression inside of parentheses to a Predicate token
                tokens.add(new Token<>(parse(insideParentheses)));

                // jump to the first character after the matching parenthesis
                i = matchingParenthesis + 1;

            } else {

                int endOfWord = findEndOfWord(expression, i);
                String word = expression.substring(i, endOfWord);
                tokens.add(toToken(word));

                // jump to the first character after the word
                i = endOfWord;
            }
        }

        return tokens;
    }

    private Predicate<T> process(List<Token<T>> tokens) {

        Operators [] operatorsInPrecedenceOrder = {NOT, AND, OR};

        for (Operators currentOperator : operatorsInPrecedenceOrder) {

            for (int i = 0; i < tokens.size(); i++) {

                Token<T> token = tokens.get(i);
                if (token.isOperator() && token.getOperator() == currentOperator) {

                    Predicate<T> rightOperand = getPredicateAtIndex(tokens, i + 1);
                    Predicate<T> leftOperand = currentOperator == NOT ? null : getPredicateAtIndex(tokens, i - 1);

                    Predicate<T> result = null;

                    switch (currentOperator) {

                        case NOT:
                            result = rightOperand.negate();
                            break;
                        case AND:
                            result = leftOperand.and(rightOperand);
                            break;
                        case OR:
                            result = leftOperand.or(rightOperand);
                            break;
                    }

                    // remove operator token
                    tokens.remove(i);
                    // remove right operand token
                    tokens.remove(i);
                    // insert result token in place of the operator
                    tokens.add(i, new Token<>(result));

                    if (currentOperator != NOT)
                        // remove left operand token
                        tokens.remove(--i);
                }
            }
        }

        if (tokens.size() == 1 && tokens.get(0).isPredicate())
            return tokens.get(0).getPredicate();
        else
            throw new RuntimeException("Unexpected tokens: " + tokens);
    }

    private Predicate<T> getPredicateAtIndex(List<Token<T>> tokenList, int index) {

        if (index < 0 || index >= tokenList.size())
            throw new RuntimeException("Operand expected");

        Token<T> token = tokenList.get(index);
        if (token.isPredicate())
            return token.getPredicate();
        else
            throw new RuntimeException("Predicate expected but operator found: " + token);
    }

    SimplePredicateParser(Function<String, Operators> operatorMapping, Function<String, Predicate<T>> predicateMapping, CasePolicy casePolicy) {

        this.operatorMapping = operatorMapping;
        this.predicateMapping = predicateMapping;
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
