package sallat.parser;

import java.util.function.Predicate;

/*
A class that encapsulates either a Predicate or an Operator
 */
class Token<T> {

    private Predicate<T> predicate;
    private Operators operator;

    public boolean isOperator() {
        return operator != null;
    }

    public boolean isPredicate() {
        return predicate != null;
    }

    public Predicate<T> getPredicate() {
        return predicate;
    }

    public Operators getOperator() {
        return operator;
    }

    public Token(Predicate<T> predicate) {
        this.predicate = predicate;
    }

    public Token(Operators operator) {
        this.operator = operator;
    }

    @Override
    public String toString() {

        return operator != null ?
                operator.toString() :
                predicate.toString();
    }
}
