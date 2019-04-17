package sallat.parser;

import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A builder used to instantiate <code>SimplePredicateParser</code>.<br>
 * @param <T> type parameter for the instance to be constructed.
 */
public class SimplePredicateParserBuilder<T> {

    private CasePolicy casePolicy;
    private Function<String, Predicate<T>> predicateMapping;
    private Function<String, Operators> operatorMapping;

    /**
     * Set case policy for the Parser to be constructed.
     * @param casePolicy <code>CasePolicy constant</code>
     * @return reference to this instance
     * @see CasePolicy
     */
    public SimplePredicateParserBuilder<T> setCasePolicy(CasePolicy casePolicy) {

        this.casePolicy = casePolicy;
        return this;
    }

    /**
     * Set the mapper wich will be used for resolving tokens to <code>Predicate</code>
     * @param predicateMapping mapper
     * @return reference for this instance.
     */
    public SimplePredicateParserBuilder<T> setPredicateMapping(Function<String, Predicate<T>> predicateMapping) {

        this.predicateMapping = predicateMapping;
        return this;
    }

    /**
     * Set the mapper wich will be used for resolving tokens to {@link Operators} constants.
     * @param operatorMapping mapper
     * @return reference for this instance.
     */
    public SimplePredicateParserBuilder<T> setOperatorMapping(Function<String, Operators> operatorMapping) {

        this.operatorMapping = operatorMapping;
        return this;
    }

    /**
     * Convenience method to set a <code>Map</code> as a predicate mapping.
     * Equivalent to <code>setPredicateMapping(predicateMap::get)</code>
     * @param predicateMap
     * @return reference to this instance
     */
    public SimplePredicateParserBuilder<T> setPredicateMap(Map<String, Predicate<T>> predicateMap) {

        this.predicateMapping = predicateMap::get;
        return this;
    }

    /**
     * Convenience method to set a <code>Map</code> as an operator mapping.
     * Equivalent to <code>setPredicateMapping(predicateMap::get)</code>
     * @param operatorMap
     * @return reference to this instance
     */
    public SimplePredicateParserBuilder<T> setOperatorMap(Map<String, Operators> operatorMap) {

        this.operatorMapping = operatorMap::get;
        return this;
    }

    /**
     * Get the constructed instance.
     * @return constructed <code>SimplePredicateParser</code>
     * @throws IllegalStateException if either property has not been set or has been set to null
     */
    public SimplePredicateParser<T> build() {

        if (predicateMapping == null)
            throw new IllegalStateException("You must either set predicateMapping or predicateMap");
        if (operatorMapping == null)
            throw new IllegalStateException("You must either set operatorMapping or operatorMap");
        if (casePolicy == null)
            throw new IllegalStateException("Case Policy must be set");

        return new SimplePredicateParser<>(operatorMapping, predicateMapping, casePolicy);
    }

}
