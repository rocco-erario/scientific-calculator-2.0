#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <cctype>
#include <cmath>
#include <stdexcept>
using namespace std;

struct Token {
    enum Type { NUMBER, OPERATOR, LPAREN, RPAREN, FUNCTION };
    Type type;
    string value;
};

bool isFunction(const string& name) {
    return name == "sqrt" || name == "sin" || name == "cos" || 
           name == "tan" || name == "sind" || name == "cosd" || name == "tand" ||
           name == "log" || name == "ln" ||
           name == "arcsen" || name == "arccos" || name == "arctan" ||
           name == "arcsend" || name == "arccosd" || name == "arctand";
}

int getPrecedence(const string& op) {
    if (op == "^") return 3;
    if (op == "*" || op == "/") return 2;
    if (op == "+" || op == "-") return 1;
    return 0;
}

bool isRightAssociative(const string& op) {
    return op == "^";
}

vector<Token> tokenize(const string& input) {
    vector<Token> tokens;
    string current = "";
    
    for (size_t i = 0; i < input.size(); i++) {
        char c = input[i];
        
        if (isspace(c)) {
            continue; // Skip whitespace
        }
        else if (isdigit(c) || c == '.') {
            current = "";
            while (i < input.size() && (isdigit(input[i]) || input[i] == '.')) {
                current += input[i];
                i++;
                }
            i--;
            tokens.push_back({Token::NUMBER, current});
        }
        else if (isalpha(c)) {
            // Build function names
            current = "";
            while (i < input.size() && isalnum(input[i])) {
                current += input[i];
                i++;
            }
            i--; // Back up one
            
            if (isFunction(current)) {
                tokens.push_back({Token::FUNCTION, current});
            } else {
                throw runtime_error("Unknown function '" + current + "'");
            }
        }
        else if (c == '(') {
            tokens.push_back({Token::LPAREN, "("});
        }
        else if (c == ')') {
            tokens.push_back({Token::RPAREN, ")"});
        }
        else if (c == '-' || c == '+') {
            // Check if this is unary minus/plus
            bool isUnary = tokens.empty() || 
                          tokens.back().type == Token::OPERATOR ||
                          tokens.back().type == Token::LPAREN;
            
            if (isUnary && c == '-') {
                // This is a unary minus - combine with next number
                i++; // Skip the minus
                if (i < input.size() && (isdigit(input[i]) || input[i] == '.')) {
                    current = "-";
                    while (i < input.size() && (isdigit(input[i]) || input[i] == '.')) {
                        current += input[i];
                        i++;
                    }
                    i--; // Back up one
                    tokens.push_back({Token::NUMBER, current});
                } else {
                    i--; // Put back the character we skipped
                    tokens.push_back({Token::OPERATOR, string(1, c)});
                }
            } else {
                tokens.push_back({Token::OPERATOR, string(1, c)});
            }
        }
        else {
            // Other operators: *, /, ^, etc.
            tokens.push_back({Token::OPERATOR, string(1, c)});
        }
    }
    
    return tokens;
}

vector<Token> insertImplicitMultiplication(vector<Token>& tokens) {
    vector<Token> result;
    
    for (size_t i = 0; i < tokens.size(); i++) {
        result.push_back(tokens[i]);
        
        // Check if we need to insert multiplication
        if (i < tokens.size() - 1) {
            Token current = tokens[i];
            Token next = tokens[i + 1];
            
            // Cases where we insert implicit multiplication:
            // NUMBER followed by LPAREN: 2(3+1)  
            // NUMBER followed by FUNCTION: 2sqrt(9)
            // RPAREN followed by NUMBER: (3+1)2
            // RPAREN followed by FUNCTION: (3+1)sqrt(9)
            // RPAREN followed by LPAREN: (3+1)(2+3)
            
            if ((current.type == Token::NUMBER && next.type == Token::LPAREN) ||
                (current.type == Token::NUMBER && next.type == Token::FUNCTION) ||
                (current.type == Token::RPAREN && next.type == Token::NUMBER) ||
                (current.type == Token::RPAREN && next.type == Token::FUNCTION) ||
                (current.type == Token::RPAREN && next.type == Token::LPAREN)) {
                
                result.push_back({Token::OPERATOR, "*"});
            }
        }
    }
    
    return result;
}

vector<Token> shuntingYard(vector<Token>& tokens){
    vector<Token> output;
    vector<Token> stack;

    for (Token token : tokens) {
        switch (token.type) {
            case Token::NUMBER:
                output.push_back(token);
                break;
            case Token::FUNCTION:
                stack.push_back(token);
                break;
            case Token::OPERATOR:
                while (!stack.empty() && 
                       stack.back().type == Token::OPERATOR &&
                       (getPrecedence(stack.back().value) > getPrecedence(token.value) ||
                        (getPrecedence(stack.back().value) == getPrecedence(token.value) && 
                         !isRightAssociative(token.value)))) {
                    output.push_back(stack.back());
                    stack.pop_back();
                }
                stack.push_back(token);
                break;
            case Token::LPAREN:
                stack.push_back(token);
                break;
            case Token::RPAREN:
                while (!stack.empty() && stack.back().type != Token::LPAREN) {
                    output.push_back(stack.back());
                    stack.pop_back();
                }
                if (!stack.empty()) {
                    stack.pop_back(); // Discard the left parenthesis
                }
                // If there's a function on top of stack, pop it too
                if (!stack.empty() && stack.back().type == Token::FUNCTION) {
                    output.push_back(stack.back());
                    stack.pop_back();
                }
                break;
        }
    }
    
    // Pop remaining operators from stack
    while (!stack.empty()) {
        output.push_back(stack.back());
        stack.pop_back();
    }
    
    return output;
}

double calc(vector<Token>& postfixTokens){
    vector<double> stack;
    const double PI = 3.14159265359;
    
    for (size_t i = 0; i < postfixTokens.size(); i++) {
        Token token = postfixTokens[i];
        
        if (token.type == Token::NUMBER) {
            stack.push_back(stod(token.value));
        } else if (token.type == Token::OPERATOR) {
            if (stack.size() < 2) {
                throw runtime_error("Not enough operands for operator " + token.value);
            }
            double right = stack.back();
            stack.pop_back();
            double left = stack.back();
            stack.pop_back();

            switch (token.value[0]) {
                case '+':
                    stack.push_back(left + right);
                    break;
                case '-':
                    stack.push_back(left - right);
                    break;
                case '*':
                    stack.push_back(left * right);
                    break;
                case '/':
                    if (right == 0) {
                        throw runtime_error("Division by zero");
                    }
                    stack.push_back(left / right);
                    break;
                case '^':
                    stack.push_back(pow(left, right));
                    break;
            }
        } else if (token.type == Token::FUNCTION) {
            if (stack.empty()) {
                throw runtime_error("Not enough operands for function " + token.value);
            }
            double operand = stack.back();
            stack.pop_back();
            
            if (token.value == "sin") {
                stack.push_back(sin(operand));
            } else if (token.value == "sind") {
                stack.push_back(sin(operand * PI / 180.0));
            } else if (token.value == "cos") {
                stack.push_back(cos(operand));
            } else if (token.value == "cosd") {
                stack.push_back(cos(operand * PI / 180.0));
            } else if (token.value == "tan") {
                stack.push_back(tan(operand));
            } else if (token.value == "tand") {
                stack.push_back(tan(operand * PI / 180.0));
            } else if (token.value == "sqrt") {
                if(operand < 0){
                    throw runtime_error("Square root of negative number");
                }
                stack.push_back(sqrt(operand));
            } else if (token.value == "log") {
                stack.push_back(log10(operand));
            } else if (token.value == "ln") {
                stack.push_back(log(operand));
            } else if (token.value == "arcsen") {
                stack.push_back(asin(operand));
            } else if (token.value == "arcsend") {
                stack.push_back(asin(operand) * 180.0 / PI);
            } else if (token.value == "arccos") {
                stack.push_back(acos(operand));
            } else if (token.value == "arccosd") {
                stack.push_back(acos(operand) * 180.0 / PI);
            } else if (token.value == "arctan") {
                stack.push_back(atan(operand));
            } else if (token.value == "arctand") {
                stack.push_back(atan(operand) * 180.0 / PI);
            }
        }
    }
    
    if (stack.size() != 1) {
        throw runtime_error("Invalid expression");
    }
    return stack.back();
}

int main() {

    cout << "Welcome to the calculator!" << endl;
    cout << "Please use the following operands ('d' stands for degrees):" <<endl;
    cout << "+, -, *, /, ^,sqrt(), sin(), cos(), tan(), sind(), cosd(), tand(), log(), ln()" << endl;
    cout << "arcsen(), arccos(), arctan(), arcsend(), arccosd(), arctand()" << endl;
    cout << endl;
while (true){
    string input;

    // Asks the user for an input
    cout << "Enter expression ('q' to quit): " << flush;
    getline(cin, input); // gets the full line
    
    if (input == "q" || input == "quit") break;
    if (input.empty()) continue;
    
    try {
        vector<Token> tokens = tokenize(input);
        tokens = insertImplicitMultiplication(tokens);
        vector<Token> postfixTokens = shuntingYard(tokens);
        double result = calc(postfixTokens);
        cout << "Result: " << result << endl;
    } catch (const exception& e) {
        cout << "Error: " << e.what() << endl;
    }
    
}
    return 0;
}
