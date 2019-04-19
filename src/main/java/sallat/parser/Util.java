package sallat.parser;

// static utility methods
class Util {

    /*
    Find index of the first whitespace character
    or left parenthesis or the end of String
    after the specified begin index.
     */
    static int findEndOfWord(String str, int beginIndex) {

        for (int i = beginIndex; i < str.length(); i++) {

            char ch = str.charAt(i);

            if (ch == '(' || Character.isWhitespace(ch))
                return i;
        }

        return str.length();
    }

    /*
    Find the index of matching right parenthesis
    given the index of left parenthesis
     */
    static int findMatchingParenthesis(String str, int beginIndex) {

        int count = 0;

        for (int i = beginIndex; i < str.length(); i++) {

            if (str.charAt(i) == '(')
                count++;

            else if (str.charAt(i) == ')')
                count--;

            if (count == 0)
                return i;
        }

        throw new RuntimeException("Unclosed parenthesis on index " + beginIndex);
    }

}
