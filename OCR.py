
# Token Types
TT_IDENTIFIER = 'IDENTIFIER'
TT_INTEGERLITERAL = 'INTEGERLITERAL'
TT_STRINGLITERAL = 'STRINGLITERAL'
TT_FLOATLITERAL = 'FLOATLITERAL'
TT_BOOLEANLITERAL = 'BOOLEANLITERAL'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MULTIPLY = 'MULTIPLY'
TT_DIVIDE = 'DIVIDE'
TT_MOD = 'MOD'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_POWER = 'POWER'
TT_AND = 'AND'
TT_OR = 'OR'
TT_NOT = 'NOT'
TT_ASSIGNMENT = 'ASSIGNMENT'
TT_EQUALITY = 'EQUALITY'
TT_NONEQUALITY = 'NONEQUALITY'
TT_LESSER = 'LESSER'
TT_LESSEREQUALITY = 'LESSEREQUALITY'
TT_GREATER = 'GREATER'
TT_GREATEREQUALITY = 'GREATEREQUALITY'
TT_COMMA = 'COMMA'
TT_IF = 'IF'
TT_THEN = 'THEN'
TT_ELSEIF = 'ELSEIF'
TT_ELSE = 'ELSE'
TT_ENDIF = 'ENDIF'
TT_SWITCH = 'SWITCH'
TT_COLON = 'COLON'
TT_CASE = 'CASE'
TT_DEFAULT = 'DEFAULT'
TT_ENDSWITCH = 'ENDSWITCH'
TT_FOR = 'FOR'
TT_TO = 'TO'
TT_NEXT = 'NEXT'
TT_WHILE = 'WHILE'
TT_ENDWHILE = 'ENDWHILE'
TT_DO = 'DO'
TT_UNTIL = 'UNTIL'
TT_PROCEDURE = 'PROCEDURE'
TT_ENDPROCEDURE = 'ENDPROCEDURE'
TT_FUNCTION = 'FUNCTION'
TT_ENDFUNCTION = 'ENDFUNCTION'
TT_RETURN = 'RETURN'
TT_NEWLINE = 'NEWLINE'

# Helper Functions

# Returns the text in a given file as a single string
def getFileText(fileName):
    with open(fileName, 'r') as file:
        return file.read()

# Returns True if tokenType in list, else False
def containsTokenType(tokenList, tokenType):
    for token in tokenList:
        if token.type == tokenType:
            return True
    return False

# Checks if TT_LPAREN count == TT_RPAREN count in a token list
def isParenBalanced(tokenList):
    leftParenCount = rightParenCount = 0
    for token in tokenList:
        if token.type == TT_LPAREN:
            leftParenCount += 1
        elif token.type == TT_RPAREN:
            rightParenCount += 1
    return leftParenCount == rightParenCount

# Checks if two tokens are equal
def equalTokens(tokOne, tokTwo):
    return tokOne.type == tokTwo.type and tokOne.value == tokTwo.value

# Represents an item of interest for parsing
class Token:
    # Type: Token type, Value: Important with data
    def __init__(self, type, value = None):
        self.type = type
        self.value = value
    # String representation of a token
    def __repr__(self):
        if self.value is None:
            return f'{self.type}'
        return f'{self.type}: {self.value}'

# Creates a token list given a text file
class Lexer:
    # Text: String of text in file, currPos: Index of text, currChar: Char at currPos
    def __init__(self, fileName):
        self.text = '\n' + getFileText(fileName)
        self.currPos = -1
        self.currChar = None
    # Decrements currPos and currChar by a single position
    def decrementByChar(self):
        self.currPos -= 1
        self.currChar = self.text[self.currPos]
    # Advances currPos and currChar by a single position
    def advanceByChar(self):
        self.currPos += 1
        if self.currPos < len(self.text):
            self.currChar = self.text[self.currPos]
        else:
            self.currChar = None
    # Returns the next character if available
    def peek(self):
        if self.currPos < len(self.text) - 1:
            return self.text[self.currPos + 1]
        return ''
    # Returns the previous character if available
    def peekBack(self):
        if self.currPos == 0:
            return ''
        return self.text[self.currPos - 1]
    # Reads text and returns the token list
    def getTokens(self):
        tokenList = []
        while True:
            self.advanceByChar()
            if self.currPos >= len(self.text):
                return tokenList
            if self.currChar in ' \t':
                pass
            elif self.currChar == '(':
                tokenList.append(Token(TT_LPAREN))
            elif self.currChar == ')':
                tokenList.append(Token(TT_RPAREN))
            elif self.currChar == '^':
                tokenList.append(Token(TT_POWER))
            elif self.currChar == '*':
                tokenList.append(Token(TT_MULTIPLY))
            elif self.currChar == '/':
                tokenList.append(Token(TT_DIVIDE))
            elif self.currChar == '+':
                tokenList.append(Token(TT_PLUS))
            elif self.currChar == '-':
                tokenList.append(Token(TT_MINUS))
            elif self.currChar == ',':
                tokenList.append(Token(TT_COMMA))
            elif self.currChar == ':':
                tokenList.append(Token(TT_COLON))
            elif self.currChar == '\n':
                spaceCount = 0
                tempPos = self.currPos
                if tempPos < len(self.text) - 1:
                    tempPos += 1
                while self.text[tempPos] == ' ':
                    spaceCount += 1
                    tempPos += 1
                    if tempPos == len(self.text):
                        break
                tokenList.append(Token(TT_NEWLINE, spaceCount))
            elif self.currChar == '=':
                nextChar = self.peek()
                if len(nextChar) == 0:
                    tokenList.append(Token(TT_ASSIGNMENT))
                else:
                    if nextChar == '=':
                        self.advanceByChar()
                        tokenList.append(Token(TT_EQUALITY))
                    else:
                        tokenList.append(Token(TT_ASSIGNMENT))
            elif self.currChar == '!':
                nextChar = self.peek()
                if len(nextChar) == 0:
                    raise SyntaxError('Invalid Syntax "!"')
                else:
                    if nextChar == '=':
                        self.advanceByChar()
                        tokenList.append(Token(TT_NONEQUALITY))
                    else:
                        raise SyntaxError('Invalid Syntax "!"')
            elif self.currChar == '<':
                nextChar = self.peek()
                if len(nextChar) == 0:
                    tokenList.append(Token(TT_LESSER))
                else:
                    if nextChar == '=':
                        self.advanceByChar()
                        tokenList.append(Token(TT_LESSEREQUALITY))
                    else:
                        tokenList.append(Token(TT_LESSER))
            elif self.currChar == '>':
                nextChar = self.peek()
                if len(nextChar) == 0:
                    tokenList.append(Token(TT_GREATER))
                else:
                    if nextChar == '=':
                        self.advanceByChar()
                        tokenList.append(Token(TT_GREATEREQUALITY))
                    else:
                        tokenList.append(Token(TT_GREATER))
            elif self.currChar == '"':
                stringValue = ''
                self.advanceByChar()
                while self.currChar != '"':
                    # Newlines, double quotes not allowed in strings
                    if self.currPos >= len(self.text) or self.currChar == '\n':
                        raise SyntaxError('" Unclosed')
                    stringValue += self.currChar
                    self.advanceByChar()
                tokenList.append(Token(TT_STRINGLITERAL, stringValue))
            elif self.currChar.isnumeric() or self.currChar == '.':
                dotCount = 0
                numValue = ''
                while self.currChar.isnumeric() or self.currChar == '.':
                    numValue += self.currChar
                    if self.currChar == '.':
                        dotCount += 1
                    self.advanceByChar()
                self.decrementByChar()
                if dotCount > 1:
                    raise SyntaxError('Invalid Number Format')
                if dotCount == 0:
                    tokenList.append(Token(TT_INTEGERLITERAL, int(numValue)))
                else:
                    tokenList.append(Token(TT_FLOATLITERAL, float(numValue)))
            elif self.currChar.isalpha() or self.currChar == '_':
                prevChar = self.peekBack()
                currWord = ''
                while self.currChar.isalpha() or self.currChar == '_' or self.currChar.isnumeric():
                    currWord += self.currChar
                    self.advanceByChar()
                self.decrementByChar()
                nextChar = self.peek()
                if currWord == 'MOD':
                    if prevChar not in ' \t' or nextChar not in ' \t':
                        raise SyntaxError('MOD Syntax Incorrect')
                    tokenList.append(Token(TT_MOD))
                elif currWord == 'DIV':
                    if prevChar not in ' \t' or nextChar not in ' \t':
                        raise SyntaxError('DIV Syntax Incorrect')
                    tokenList.append(Token(TT_DIV))
                elif currWord == 'AND':
                    if prevChar not in ' \t' or nextChar not in ' \t':
                        raise SyntaxError('AND Syntax Incorrect')
                    tokenList.append(Token(TT_AND))
                elif currWord == 'OR':
                    if prevChar not in ' \t' or nextChar not in ' \t':
                        raise SyntaxError('OR Syntax Incorrect')
                    tokenList.append(Token(TT_OR))
                elif currWord == 'NOT':
                    if prevChar not in ' (\t,' or nextChar not in ' \t':
                        raise SyntaxError('NOT Syntax Incorrect')
                    tokenList.append(Token(TT_NOT))
                elif currWord == 'if':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('if Syntax Incorrect')
                    tokenList.append(Token(TT_IF))
                elif currWord == 'then':
                    if prevChar not in ' \t' or nextChar not in ' \t\n':
                        raise SyntaxError('then Syntax Incorrect')
                    tokenList.append(Token(TT_THEN))
                elif currWord == 'elseif':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('elseif Syntax Incorrect')
                    tokenList.append(Token(TT_ELSEIF))
                elif currWord == 'else':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('else Syntax Incorrect')
                    tokenList.append(Token(TT_ELSE))
                elif currWord == 'endif':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('endif Syntax Incorrect')
                    tokenList.append(Token(TT_ENDIF))
                elif currWord == 'switch':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('switch Syntax Incorrect')
                    tokenList.append(Token(TT_SWITCH))
                elif currWord == 'case':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('case Syntax Incorrect')
                    tokenList.append(Token(TT_CASE))
                elif currWord == 'default':
                    if prevChar not in ' \t\n' or nextChar not in ' \t:':
                        raise SyntaxError('default Syntax Incorrect')
                    tokenList.append(Token(TT_DEFAULT))
                elif currWord == 'endswitch':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('endswitch Syntax Incorrect')
                    tokenList.append(Token(TT_ENDSWITCH))
                elif currWord == 'for':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('for Syntax Incorrect')
                    tokenList.append(Token(TT_FOR))
                elif currWord == 'to':
                    if prevChar not in ' \t' or nextChar not in ' \t':
                        raise SyntaxError('to Syntax Incorrect')
                    tokenList.append(Token(TT_TO))
                elif currWord == 'next':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('next Syntax Incorrect')
                    tokenList.append(Token(TT_NEXT))
                elif currWord == 'while':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('while Syntax Incorrect')
                    tokenList.append(Token(TT_WHILE))
                elif currWord == 'endwhile':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('endwhile Syntax Incorrect')
                    tokenList.append(Token(TT_ENDWHILE))
                elif currWord == 'do':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('do Syntax Incorrect')
                    tokenList.append(Token(TT_DO))
                elif currWord == 'until':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('until Syntax Incorrect')
                    tokenList.append(Token(TT_UNTIL))
                elif currWord == 'procedure':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('procedure Syntax Incorrect')
                    tokenList.append(Token(TT_PROCEDURE))
                elif currWord == 'endprocedure':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('endprocedure Syntax Incorrect')
                    tokenList.append(Token(TT_ENDPROCEDURE))
                elif currWord == 'function':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('function Syntax Incorrect')
                    tokenList.append(Token(TT_FUNCTION))
                elif currWord == 'endfunction':
                    if prevChar not in ' \t\n' or nextChar not in ' \t\n':
                        raise SyntaxError('endfunction Syntax Incorrect')
                    tokenList.append(Token(TT_ENDFUNCTION))
                elif currWord == 'return':
                    if prevChar not in ' \t\n' or nextChar not in ' \t':
                        raise SyntaxError('return Syntax Incorrect')
                    tokenList.append(Token(TT_RETURN))
                elif currWord == 'True':
                    if prevChar not in ' ,\t(' or nextChar not in ' \t,):\n':
                        raise SyntaxError('Boolean True Syntax Incorrect')
                    tokenList.append(Token(TT_BOOLEANLITERAL, True))
                elif currWord == 'False':
                    if prevChar not in ' ,\t(' or nextChar not in ' ,\t):\n':
                        raise SyntaxError('Boolean False Syntax Incorrect')
                    tokenList.append(Token(TT_BOOLEANLITERAL, False))
                else:
                    if prevChar.isnumeric():
                        raise SyntaxError('Identifer Syntax Incorrect')
                    tokenList.append(Token(TT_IDENTIFIER, currWord))

# Parser specifically for expressions
class ExpressionParser:
    # Same attributes as main parser
    def __init__(self, tokenList):
        if len(tokenList) == 0:
            raise SyntaxError('Invalid Expression Syntax')
        if not isParenBalanced(tokenList):
            raise SyntaxError('Unclosed/Unopened Bracket(s)')
        self.tokenList = tokenList
        self.currPos = -1
        self.currToken = None
        self.bracketCount = 0
        self.termOrder = (
            [TT_LESSER, TT_LESSEREQUALITY, TT_GREATER, TT_GREATEREQUALITY],
            [TT_OR, TT_PLUS, TT_MINUS],
            [TT_MULTIPLY, TT_DIVIDE, TT_MOD, TT_DIV, TT_AND],
            [TT_POWER]
        )
        self.advanceToken()
    # Same functionality as main parser
    def decrementToken(self):
        self.currPos -= 1
        self.currToken = self.tokenList[self.currPos]
    # Same functionality as main parser
    def advanceToken(self):
        self.currPos += 1
        if self.currPos < len(self.tokenList):
            self.currToken = self.tokenList[self.currPos]
        else:
            self.currToken = None
    # Parses a general term
    def parseGeneralTerm(self, termIndex):
        factorFlag = False
        if termIndex == len(self.termOrder) - 1:
            factorFlag = True
        leftNode = self.parseGeneralTerm(termIndex + 1) if not factorFlag else self.parseFactor()
        self.advanceToken()
        while self.currPos < len(self.tokenList):
            if self.currToken.type not in self.termOrder[termIndex]:
                self.decrementToken()
                return leftNode
            opToken = self.currToken
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Invalid Expression Syntax')
            rightNode = self.parseGeneralTerm(termIndex + 1) if not factorFlag else self.parseFactor()
            leftNode = BinaryOpNode(leftNode, opToken, rightNode)
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                self.decrementToken()
                return leftNode
            if self.currToken == TT_RPAREN:
                self.decrementToken()
                return leftNode
        self.decrementToken()
        return leftNode
    # Parses an expression
    def parseExpression(self):
        leftNode = self.parseGeneralTerm(0)
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            return leftNode
        if self.currToken.type == TT_RPAREN:
            self.decrementToken()
            return leftNode
        while self.currPos < len(self.tokenList):
            if self.currToken.type not in  (TT_EQUALITY, TT_NONEQUALITY):
                raise SyntaxError('Invalid Expression Syntax')
            opToken = self.currToken
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Invalid Expression Syntax')
            rightNode = self.parseGeneralTerm(0)
            leftNode = BinaryOpNode(leftNode, opToken, rightNode)
            self.advanceToken()
            if self.currPos < len(self.tokenList):
                if self.currToken.type == TT_RPAREN and self.bracketCount > 0:
                    self.bracketCount -= 1
                    self.decrementToken()
                    return leftNode
                elif self.currToken.type == TT_RPAREN and self.bracketCount == 0:
                    raise SyntaxError('Bracket Unopened')
        self.decrementToken()
        return leftNode
    # Parses an individual factor
    def parseFactor(self):
        if self.currToken.type == TT_LPAREN:
            self.bracketCount += 1
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Bracket Unclosed')
            tempNode = self.parseExpression()
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Bracket Unclosed')
            if self.currToken.type != TT_RPAREN:
                raise SyntaxError('Bracket Unclosed')
            return tempNode
        elif self.currToken.type in (TT_INTEGERLITERAL, TT_FLOATLITERAL):
            return NumberLiteralNode(self.currToken)
        elif self.currToken.type == TT_BOOLEANLITERAL:
            return BooleanLiteralNode(self.currToken)
        elif self.currToken.type == TT_STRINGLITERAL:
            return StringLiteralNode(self.currToken)
        elif self.currToken.type in (TT_PLUS, TT_MINUS, TT_NOT):
            operatorSign = self.getOperatorSign()
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Invalid Expression Syntax')
            tempNode = None
            if self.currToken.type == TT_LPAREN:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Bracket Unclosed')
                tempNode = self.parseExpression()
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Bracket Unclosed')
                if self.currToken.type != TT_RPAREN:
                    raise SyntaxError('Bracket Unclosed')
            else:
                if self.currToken.type in (TT_INTEGERLITERAL, TT_FLOATLITERAL):
                    tempNode = NumberLiteralNode(self.currToken)
                elif self.currToken.type == TT_BOOLEANLITERAL:
                    tempNode = BooleanLiteralNode(self.currToken)
                elif self.currToken.type == TT_IDENTIFER:
                    tempNode = self.parseIdentifer()
                elif self.currToken.type == TT_STRINGLITERAL:
                    tempNode = StringLiteralNode(self.currToken)
                else:
                    raise SyntaxError('Invalid Syntax')
            if operatorSign.type in (TT_MINUS, TT_NOT):
                return UnaryOpNode(operatorSign, tempNode)
            else:
                return tempNode
        elif self.currToken.type == TT_IDENTIFIER:
            return self.parseIdentifier()
        else:
            raise SyntaxError('Invalid Expression Syntax')
    # Parses an identifier (may be a function)
    def parseIdentifier(self):
        nameToken = self.currToken
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            self.decrementToken()
            return VariableNode(nameToken)
        if self.currToken.type != TT_LPAREN:
            self.decrementToken()
            return VariableNode(nameToken)
        parameterNodeList = []
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Bracket Unclosed')
        while self.currToken.type != TT_RPAREN:
            tempTokenList = []
            tempBracketCount = 0
            while True:
                if self.currToken.type == TT_LPAREN:
                    tempBracketCount += 1
                elif self.currToken.type == TT_RPAREN:
                    tempBracketCount -= 1
                tempTokenList.append(self.currToken)
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Bracket Unclosed')
                if self.currToken.type in (TT_RPAREN, TT_COMMA) and tempBracketCount == 0:
                    break
                if self.currToken.type == TT_RPAREN and tempBracketCount <= 0:
                    raise SyntaxError('Unopened Bracket')
            if len(tempTokenList) == 0:
                raise SyntaxError('Parameter Intended But Not Found')
            currParameterNode = ExpressionParser(tempTokenList).parseExpression()
            parameterNodeList.append(currParameterNode)
            if self.currToken.type == TT_COMMA:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Bracket Unclosed')
        return FunctionCallNode(nameToken, parameterNodeList)
    # Goes through contiguous operators and determines the effective sign
    def getOperatorSign(self):
        operatorSign = 1
        expressionType = 'Unknown'
        while self.currToken.type in (TT_PLUS, TT_MINUS, TT_NOT):
            if self.currToken.type in (TT_PLUS, TT_MINUS):
                expressionType = 'Number'
            else:
                expressionType = 'Boolean'
            if self.currToken.type in (TT_PLUS, TT_MINUS) and expressionType == 'Boolean':
                raise SyntaxError('Invalid Operand Types in Expression')
            if self.currToken.type == TT_NOT and expressionType == 'Number':
                raise SyntaxError('Invalid Operand Types in Expression')
            if self.currToken.type in (TT_MINUS, TT_NOT):
                operatorSign *= -1
            self.advanceToken()
        self.decrementToken()
        if expressionType == 'Number' and operatorSign == -1:
            return Token(TT_MINUS)
        elif expressionType == 'Boolean' and operatorSign == -1:
            return Token(TT_NOT)
        return Token(TT_PLUS)

# Creates an abstract syntax tree given a token list
class Parser:
    # tokenList: complete list of tokens, currPos: current index, currToken: token at currPos
    def __init__(self, tokenList):
        self.tokenList = tokenList
        self.currPos = -1
        self.currToken = None
        self.advanceToken()
    # Decrements currPos and currToken by a single position
    def decrementToken(self):
        self.currPos -= 1
        self.currToken = self.tokenList[self.currPos]
    # Advances currPos and currToken by a single position
    def advanceToken(self):
        self.currPos += 1
        if self.currPos < len(self.tokenList):
            self.currToken = self.tokenList[self.currPos]
        else:
            self.currToken = None
    # Builds the statementList/is the entry point
    def parseProgram(self):
        programNode = ProgramNode()
        while self.currPos < len(self.tokenList):
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    return programNode
            self.decrementToken()
            if self.currToken.value != 0:
                raise IndentationError('Invalid Indentation')
            self.advanceToken()
            programNode.addStatement(self.parseStatement())
        return programNode
    # Determines current statement type and transfers parsing
    def parseStatement(self):
        # Determining statement type
        tempTokenList = []
        tempPos = self.currPos
        while self.tokenList[tempPos].type != TT_NEWLINE:
            tempTokenList.append(self.tokenList[tempPos])
            tempPos += 1
        self.decrementToken()
        # Transferring parsing
        if containsTokenType(tempTokenList, TT_IF):
            return self.parseIfStatement() # Done
        elif containsTokenType(tempTokenList, TT_RETURN):
            return self.parseReturnStatement() # Done
        elif containsTokenType(tempTokenList, TT_SWITCH):
            return self.parseSwitchStatement()
        elif containsTokenType(tempTokenList, TT_FOR):
            return self.parseForStatement() # Done
        elif containsTokenType(tempTokenList, TT_WHILE):
            return self.parseWhileStatement() # Done
        elif containsTokenType(tempTokenList, TT_DO):
            return self.parseDoUntilStatement() # Done
        elif containsTokenType(tempTokenList, TT_FUNCTION):
            return self.parseFunctionDeclarationStatement() # Done
        elif containsTokenType(tempTokenList, TT_PROCEDURE):
            return self.parseProcedureDeclarationStatement() # Done
        elif containsTokenType(tempTokenList, TT_ASSIGNMENT):
            return self.parseVariableDeclarationStatement() # Done
        elif containsTokenType(tempTokenList, TT_IDENTIFIER):
            return self.parseProcedureCallStatement() # Done
        else:
            raise SyntaxError('Line Does Not Meet Available Statement Type')
    # Parses an if statement
    def parseIfStatement(self):
        spaceCount = self.currToken.value
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        ifStatementNode = IfStatementNode()
        branchNode = self.parseIfBranch(TT_IF, spaceCount)
        if len(branchNode.statementList) == 0:
            raise SyntaxError('If Syntax Incorrect')
        ifStatementNode.addBranch(branchNode)
        while True:
            newSpaceCount = self.currToken.value
            if newSpaceCount != spaceCount:
                raise IndentationError('Invalid Indentation')
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
            elif self.currToken.type == TT_ELSEIF:
                ifStatementNode.addBranch(self.parseIfBranch(TT_ELSEIF, spaceCount))
            elif self.currToken.type == TT_ELSE:
                ifStatementNode.addBranch(self.parseIfBranch(TT_ELSE, spaceCount))
                newSpaceCount = self.currToken.value
                if newSpaceCount != spaceCount:
                    raise IndentationError('Invalid Indentation')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_ENDIF:
                    raise SyntaxError('If Syntax Incorrect')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_NEWLINE:
                    raise SyntaxError('If Syntax Incorrect')
                return ifStatementNode
            elif self.currToken.type == TT_ENDIF:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_NEWLINE:
                    raise SyntaxError('If Syntax Incorrect')
                return ifStatementNode
    # Parses a branch of an if statement
    def parseIfBranch(self, ifTokenType, prevSpaceCount):
        if self.currToken.type != ifTokenType:
            raise SyntaxError('Invalid Syntax')
        terminatingFlag = ifTokenType != TT_ELSE
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        tempTokenList = []
        condition = None
        if terminatingFlag:
            while self.currToken.type != TT_THEN:
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type == TT_NEWLINE:
                    raise SyntaxError('If Syntax Incorrect')
                tempTokenList.append(self.currToken)
                self.advanceToken()
            if len(tempTokenList) == 0:
                raise SyntaxError('If Expression Not Found')
            condition = ExpressionParser(tempTokenList).parseExpression()
            self.advanceToken()
        else:
            condition = BooleanLiteralNode(Token(TT_BOOLEANLITERAL, True))
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_NEWLINE:
            raise SyntaxError('If Syntax Incorrect')
        branchNode = BranchNode(condition)
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Unfinished If Statement')
        if self.currToken.type == TT_ENDIF:
            raise SyntaxError('Empty If Block')
        self.decrementToken()
        standardSpaceCount = self.currToken.value
        if standardSpaceCount <= prevSpaceCount:
            raise IndentationError('Invalid Indentation')
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Unfinished If Statement')
            if self.currToken.type in (TT_ENDIF, TT_ELSEIF, TT_ELSE):
                self.decrementToken()
                return branchNode
            self.decrementToken()
            spaceCount = self.currToken.value
            if spaceCount != standardSpaceCount:
                raise IndentationError('Invalid Indentation')
            self.advanceToken()
            branchNode.addStatement(self.parseStatement())
    # Parses a variable declaration statement
    def parseVariableDeclarationStatement(self):
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_IDENTIFIER:
            raise SyntaxError('Assignment Syntax Invalid')
        identifierNode = VariableNode(self.currToken)
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_ASSIGNMENT:
            raise SyntaxError('Assignment Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        tempTokenList = []
        while self.currToken.type != TT_NEWLINE:
            tempTokenList.append(self.currToken)
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
        expressionNode = ExpressionParser(tempTokenList).parseExpression()
        return VariableDeclarationStatementNode(identifierNode, expressionNode)
    # Parses a procedure call statement
    def parseProcedureCallStatement(self):
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_IDENTIFIER:
            raise SyntaxError('Assignment Syntax Invalid')
        tempTokenList = []
        while self.currToken.type != TT_NEWLINE:
            tempTokenList.append(self.currToken)
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
        self.decrementToken()
        if self.currToken.type != TT_RPAREN:
            raise SyntaxError('Procedure Call Syntax Invalid')
        self.advanceToken()
        functionMirrorNode = ExpressionParser(tempTokenList).parseIdentifier()
        try:
            functionMirrorNode.parameterNodeList
        except AttributeError:
            raise SyntaxError('Procedure Call Syntax Invalid')
        return ProcedureCallNode(functionMirrorNode.nameToken, functionMirrorNode.parameterNodeList)
    # Parses a function declaration statement by transferring control to the subroutine parser
    def parseFunctionDeclarationStatement(self):
        return self.parseSubroutineDeclarationStatement(TT_FUNCTION, TT_ENDFUNCTION, 'Function')
    def parseProcedureDeclarationStatement(self):
        return self.parseSubroutineDeclarationStatement(TT_PROCEDURE, TT_ENDPROCEDURE, 'Procedure')
    # Parses a general subroutine declaration statement
    def parseSubroutineDeclarationStatement(self, startType, endType, nameString):
        primarySpaceCount = self.currToken.value
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != startType:
            raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_IDENTIFIER:
            raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
        nameToken = self.currToken
        subroutineDeclarationNode = None
        if startType == TT_FUNCTION:
            subroutineDeclarationNode = FunctionDeclarationNode(nameToken)
        elif startType == TT_PROCEDURE:
            subroutineDeclarationNode = ProcedureDeclarationNode(nameToken)
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_LPAREN:
            raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Invalid Syntax')
        parameterNameTokenList = []
        while self.currToken.type != TT_RPAREN:
            if self.currToken.type == TT_NEWLINE:
                raise SyntaxError('Bracket Unclosed')
            if self.currToken.type != TT_IDENTIFIER:
                raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
            subroutineDeclarationNode.addParameterNameToken(self.currToken)
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
            if self.currToken.type == TT_COMMA:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Bracket Unclosed')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_NEWLINE:
            raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
        if self.currToken.type == endType:
            raise SyntaxError(f'Empty {nameString} Declaration')
        self.decrementToken()
        standardSpaceCount = self.currToken.value
        if standardSpaceCount <= primarySpaceCount:
            raise IndentationError('Indentation Invalid')
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
            if self.currToken.type == endType:
                self.decrementToken()
                terminatingSpaceCount = self.currToken.value
                self.advanceToken()
                if terminatingSpaceCount != primarySpaceCount:
                    raise IndentationError(f'{nameString} Declaration Syntax Invalid')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_NEWLINE:
                    raise SyntaxError(f'{nameString} Declaration Syntax Invalid')
                return subroutineDeclarationNode
            self.decrementToken()
            spaceCount = self.currToken.value
            if spaceCount != standardSpaceCount:
                raise IndentationError('Invalid Indentation')
            self.advanceToken()
            subroutineDeclarationNode.addStatement(self.parseStatement())
    # Parses return statement
    def parseReturnStatement(self):
        self.advanceToken()
        if self.currToken.type != TT_RETURN:
            raise SyntaxError('Return Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        tempTokenList = []
        while self.currToken.type != TT_NEWLINE:
            tempTokenList.append(self.currToken)
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Return Syntax Invalid')
        if len(tempTokenList) == 0:
            raise SyntaxError('Return Expression Not Found')
        returnExpression = ExpressionParser(tempTokenList).parseExpression()
        return ReturnNode(returnExpression)
    # Parses while statement
    def parseWhileStatement(self):
        primarySpaceCount = self.currToken.value
        self.advanceToken()
        if self.currToken.type != TT_WHILE:
            raise SyntaxError('While Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        tempTokenList = []
        while self.currToken.type != TT_NEWLINE:
            tempTokenList.append(self.currToken)
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('While Syntax Invalid')
        if len(tempTokenList) == 0:
            raise SyntaxError('While Expression Not Found')
        condition = ExpressionParser(tempTokenList).parseExpression()
        whileStatementNode = WhileStatementNode(condition)
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Unfinished While Loop')
        if self.currToken.type == TT_ENDWHILE:
            raise SyntaxError('Empty While')
        self.decrementToken()
        standardSpaceCount = self.currToken.value
        if standardSpaceCount <= primarySpaceCount:
            raise IndentationError('Indentation Invalid')
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Unfinished While Loop')
            if self.currToken.type == TT_ENDWHILE:
                self.decrementToken()
                terminatingSpaceCount = self.currToken.value
                self.advanceToken()
                if terminatingSpaceCount != primarySpaceCount:
                    raise IndentationError('Indentation Invalid')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_NEWLINE:
                    raise SyntaxError('While Syntax Invalid')
                return whileStatementNode
            self.decrementToken()
            spaceCount = self.currToken.value
            if spaceCount != standardSpaceCount:
                raise IndentationError('Indentation Invalid')
            self.advanceToken()
            whileStatementNode.addStatement(self.parseStatement())
    # Parses for statement
    def parseForStatement(self):
        primarySpaceCount = self.currToken.value
        self.advanceToken()
        if self.currToken.type != TT_FOR:
            raise SyntaxError('For Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_IDENTIFIER:
            raise SyntaxError('For Syntax Invalid')
        iterNameToken = self.currToken
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_ASSIGNMENT:
            raise SyntaxError('For Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        tempTokenList = []
        while self.currToken.type != TT_TO:
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
            if self.currToken.type == TT_NEWLINE:
                raise SyntaxError('For Syntax Invalid')
            tempTokenList.append(self.currToken)
            self.advanceToken()
        startNode = ExpressionParser(tempTokenList).parseExpression()
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        tempTokenList = []
        while self.currToken.type != TT_NEWLINE:
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
            tempTokenList.append(self.currToken)
            self.advanceToken()
        endNode = ExpressionParser(tempTokenList).parseExpression()
        forStatementNode = ForStatementNode(startNode, endNode, iterNameToken)
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Unfinished For Loop')
        if self.currToken.type == TT_NEXT:
            raise SyntaxError('Empty For')
        self.decrementToken()
        standardSpaceCount = self.currToken.value
        if standardSpaceCount <= primarySpaceCount:
            raise IndentationError('Indentation Invalid')
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Unfinished For Loop')
            if self.currToken.type == TT_NEXT:
                self.decrementToken()
                terminatingSpaceCount = self.currToken.value
                self.advanceToken()
                if terminatingSpaceCount != primarySpaceCount:
                    raise IndentationError('Indentation Invalid')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if not equalTokens(self.currToken, iterNameToken):
                    raise SyntaxError('For Loop Must Begin And Terminate With The Same Variable')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_NEWLINE:
                    raise SyntaxError('For Syntax Invalid')
                return forStatementNode
            self.decrementToken()
            spaceCount = self.currToken.value
            if spaceCount != standardSpaceCount:
                raise IndentationError('Indentation Invalid')
            self.advanceToken()
            forStatementNode.addStatement(self.parseStatement())
    def parseDoUntilStatement(self):
        primarySpaceCount = self.currToken.value
        self.advanceToken()
        if self.currToken.type != TT_DO:
            raise SyntaxError('Do Until Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_NEWLINE:
            raise SyntaxError('Newline Missing At EOF')
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Unfinished Do Until Loop')
        if self.currToken.type == TT_UNTIL:
            raise SyntaxError('Empty Do Until')
        self.decrementToken()
        standardSpaceCount = self.currToken.value
        if standardSpaceCount <= primarySpaceCount:
            raise IndentationError('Indentation Invalid')
        doUntilStatementNode = DoUntilStatementNode()
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Unfinished Do Until Loop')
            if self.currToken.type == TT_UNTIL:
                self.decrementToken()
                terminatingSpaceCount = self.currToken.value
                self.advanceToken()
                if terminatingSpaceCount != primarySpaceCount:
                    raise IndentationError('Indentation Invalid')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                tempTokenList = []
                while self.currToken.type != TT_NEWLINE:
                    if self.currPos >= len(self.tokenList):
                        raise SyntaxError('Newline Missing At EOF')
                    tempTokenList.append(self.currToken)
                    self.advanceToken()
                doUntilStatementNode.addCondition(ExpressionParser(tempTokenList).parseExpression())
                return doUntilStatementNode
            self.decrementToken()
            spaceCount = self.currToken.value
            if spaceCount != standardSpaceCount:
                raise IndentationError('Indentation Invalid')
            self.advanceToken()
            doUntilStatementNode.addStatement(self.parseStatement())
    # Parses a switch statement
    def parseSwitchStatement(self):
        primarySpaceCount = self.currToken.value
        self.advanceToken()
        if self.currToken.type != TT_SWITCH:
            raise SyntaxError('Switch Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_IDENTIFIER:
            raise SyntaxError('Switch Syntax Invalid')
        varToken = self.currToken
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_COLON:
            raise SyntaxError('Switch Syntax Invalid')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_NEWLINE:
            raise SyntaxError('Switch Syntax Invalid')
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Switch Statement Incomplete')
        if self.currToken.type == TT_ENDSWITCH:
            raise SyntaxError('Empty Switch')
        self.decrementToken()
        switchStatementNode = SwitchStatementNode()
        secondarySpaceCount = self.currToken.value
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Switch Syntax Incomplete')
            if self.currToken.type == TT_ENDSWITCH:
                self.decrementToken()
                terminatingSpaceCount = self.currToken.value
                self.advanceToken()
                if terminatingSpaceCount != primarySpaceCount:
                    raise IndentationError('Indentation Invalid')
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
                if self.currToken.type != TT_NEWLINE:
                    raise SyntaxError('Switch Syntax Invalid')
                if len(switchStatementNode.branchList) == 0:
                    raise SyntaxError('No Switch Statement Body')
                return switchStatementNode
            elif self.currToken.type == TT_CASE:
                self.decrementToken()
                spaceCount = self.currToken.value
                if spaceCount != secondarySpaceCount:
                    raise IndentationError('Indentation Invalid')
                switchStatementNode.addBranch(self.parseSwitchBranch(varToken))
            elif self.currToken.type == TT_DEFAULT:
                self.decrementToken()
                spaceCount = self.currToken.value
                if spaceCount != secondarySpaceCount:
                    raise IndentationError('Indentation Invalid')
                switchStatementNode.addBranch(self.parseSwitchBranch(varToken))
                self.advanceToken()
                if self.currToken.type != TT_ENDSWITCH:
                    raise SyntaxError('Switch Default Syntax Invalid')
                self.decrementToken()
            else:
                raise SyntaxError('Switch Syntax Invalid')
    # Parses a switch branch
    def parseSwitchBranch(self, varToken):
        primarySpaceCount = self.currToken.value
        self.advanceToken()
        if self.currToken.type not in (TT_CASE, TT_DEFAULT):
            raise SyntaxError('Switch Syntax Invalid')
        conditionExpression = None
        if self.currToken.type == TT_CASE:
            self.advanceToken()
            tempTokenList = []
            while self.currToken.type != TT_COLON:
                if self.currToken.type == TT_NEWLINE:
                    raise SyntaxError('No Colon Ending Case')
                tempTokenList.append(self.currToken)
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Newline Missing At EOF')
            if len(tempTokenList) == 0:
                raise SyntaxError('Case Expression Missing')
            self.decrementToken()
            conditionExpression = BinaryOpNode(VariableNode(varToken), TT_EQUALITY, ExpressionParser(tempTokenList).parseExpression())
        else:
            conditionExpression = BooleanLiteralNode(Token(TT_BOOLEANLITERAL, True))
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_COLON:
            raise SyntaxError('No Colon Ending Case/Default')
        self.advanceToken()
        if self.currPos >= len(self.tokenList):
            raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type != TT_NEWLINE:
            raise SyntaxError('Switch Syntax Invalid')
        branchNode = BranchNode(conditionExpression)
        while self.currToken.type == TT_NEWLINE:
            self.advanceToken()
            if self.currPos >= len(self.tokenList):
                raise SyntaxError('Newline Missing At EOF')
        if self.currToken.type in (TT_DEFAULT, TT_CASE, TT_ENDSWITCH):
            raise SyntaxError('No Case/Default Body')
        self.decrementToken()
        standardSpaceCount = self.currToken.value
        if standardSpaceCount <= primarySpaceCount:
            raise IndentationError('Indentation Invalid')
        while True:
            while self.currToken.type == TT_NEWLINE:
                self.advanceToken()
                if self.currPos >= len(self.tokenList):
                    raise SyntaxError('Switch Statement Incomplete')
            if self.currToken.type in (TT_CASE, TT_DEFAULT, TT_ENDSWITCH):
                self.decrementToken()
                return branchNode
            self.decrementToken()
            spaceCount = self.currToken.value
            if spaceCount != standardSpaceCount:
                raise IndentationError('Indentation Invalid')
            self.advanceToken()
            branchNode.addStatement(self.parseStatement())

# Root node of the program in the AST
class ProgramNode:
    # statementList: list of statements to interpret in order
    def __init__(self):
        self.statementList = []
    # String representation of a ProgramNode
    def __repr__(self):
        return f'(PROGRAM: {self.statementList})'
    # Appends a StatementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)

# Represents an if statement in the AST
class IfStatementNode:
    # branchList: List of branches in order
    def __init__(self):
        self.branchList = []
    # String representation of IfStatementNode
    def __repr__(self):
        return f'<IF: {self.branchList}>'
    # Appends a branchNode to the branchList
    def addBranch(self, branchNode):
        self.branchList.append(branchNode)

# Represents a switch statement in the AST
class SwitchStatementNode:
    # branchList: List of potential branches
    def __init__(self):
        self.branchList = []
    # String representation of SwitchStatementNode
    def __repr__(self):
        return f'<SWITCH: {self.branchList}>'
    # Appends a branchNode to the branchList
    def addBranch(self, branchNode):
        self.branchList.append(branchNode)

# Represents a while loop in the AST
class WhileStatementNode:
    # condition: Must be satisfied to execute statementList, statementList: List to execute
    def __init__(self, condition):
        self.condition = condition
        self.statementList = []
    # String representation of WhileStatementNode
    def __repr__(self):
        return f'<WHILE: {self.condition} -> {self.statementList}>'
    # Appends a statementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)

# Represents a for loop in the AST
class ForStatementNode:
    # startNode: Initial counter value, endNode: End counter value, iterNameToken: Counter identifier token, statementList: List of statements to execute
    def __init__(self, startNode, endNode, iterNameToken):
        self.startNode = startNode
        self.endNode = endNode
        self.iterNameToken = iterNameToken
        self.statementList = []
    # String representation of ForStatementNode
    def __repr__(self):
        return f'<FOR: ITER: {self.startNode} - {self.endNode}; STATEMENTS: {self.statementList}>'
    # Appends a statementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)

# Represents a do-until loop in the AST
class DoUntilStatementNode:
    # condition: Execution ends when satisfied, statementList: List of statements to execute
    def __init__(self):
        self.condition = None
        self.statementList = []
    # String representation of DoUntilStatementNode
    def __repr__(self):
        return f'<DO UNTIL: {self.condition} -> {self.statementList}>'
    # Appends a statementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)
    # Assigns the condition
    def addCondition(self, conditionNode):
        self.condition = conditionNode

# Represents a variable declaration statement in the AST
class VariableDeclarationStatementNode:
    # identifierNode: Node of identifier, expressionNode: Node of expression
    def __init__(self, identifierNode, expressionNode):
        self.identifierNode = identifierNode
        self.expressionNode = expressionNode
    # String representation of VariableDeclarationStatementNode
    def __repr__(self):
        return f'<ASSIGNMENT: {self.identifierNode} = {self.expressionNode}>'

# Represents a procedure call in the AST
class ProcedureCallNode:
    # nameToken: Primary identifier, parameterTokenList: List of parameter expressions
    def __init__(self, nameToken, parameterNodeList):
        self.nameToken = nameToken
        self.parameterNodeList = parameterNodeList
    # String representation of FunctionCallNode
    def __repr__(self):
        return f'<PROCEDURE: {self.nameToken.value}; PARAMS: {self.parameterNodeList}>'

# Represents a function declaration in the AST
class FunctionDeclarationNode:
    # nameToken: Primary identifier, parameterNameTokenList: Parameter name tokens, statementList: List of statements
    def __init__(self, nameToken):
        self.nameToken = nameToken
        self.parameterNameTokenList = []
        self.statementList = []
    # String representation of FunctionDeclarationNode
    def __repr__(self):
        return f'<FUNC DECLARE: {self.nameToken.value}; PARAMS: {self.parameterNameTokenList}; STATEMENTS: {self.statementList}>'
    # Appends a parameterNameToken to the parameterNameTokenList
    def addParameterNameToken(self, parameterNameToken):
        self.parameterNameTokenList.append(parameterNameToken)
    # Appends a statementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)

# Represents a return statement in the AST
class ReturnNode:
    # returnExpression: expression to be returned by function call
    def __init__(self, returnExpression):
        self.returnExpression = returnExpression
    # String representation of ReturnNode
    def __repr__(self):
        return f'<RETURN: {self.returnExpression}>'

# Represents a function declaration in the AST
class ProcedureDeclarationNode:
    # nameToken: Primary identifier, parameterNameTokenList: Parameter name tokens, statementList: List of statements
    def __init__(self, nameToken):
        self.nameToken = nameToken
        self.parameterNameTokenList = []
        self.statementList = []
    # String representation of FunctionDeclarationNode
    def __repr__(self):
        return f'<PROC DECLARE: {self.nameToken.value}; PARAMS: {self.parameterNameTokenList}; STATEMENTS: {self.statementList}>'
    # Appends a parameterNameToken to the parameterNameTokenList
    def addParameterNameToken(self, parameterNameToken):
        self.parameterNameTokenList.append(parameterNameToken)
    # Appends a statementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)

# Represents a branch in the AST
class BranchNode:
    # condition: Must be satisfied to execute statementList, statementList: List to execute
    def __init__(self, condition):
        self.condition = condition
        self.statementList = []
    # String representation of BranchNode
    def __repr__(self):
        return f'(BRANCH: {self.condition} -> {self.statementList})'
    # Appends a statementNode to the statementList
    def addStatement(self, statementNode):
        self.statementList.append(statementNode)

# Represents a number literal in the AST
class NumberLiteralNode:
    # numToken: token of number
    def __init__(self, numToken):
        self.numToken = numToken
    # String representation of a NumberLiteralNode
    def __repr__(self):
        return f'{self.numToken}'

# Represents a boolean literal in the AST
class BooleanLiteralNode:
    # boolToken: token of boolean
    def __init__(self, boolToken):
        self.boolToken = boolToken
    # String representation of a BooleanLiteralNode
    def __repr__(self):
        return f'{self.boolToken}'

# Represents a string literal in the AST
class StringLiteralNode:
    # strToken: token of string
    def __init__(self, strToken):
        self.strToken = strToken
    # String representation of a StringLiteralNode
    def __repr__(self):
        return f'{self.strToken}'

# Represents a variable node in the AST
class VariableNode:
    # nameToken: primary identifier
    def __init__(self, nameToken):
        self.nameToken = nameToken
    # String representation of VariableNode
    def __repr__(self):
        return f'{self.nameToken}'

# Represents a function call in the AST
class FunctionCallNode:
    # nameToken: primary identifier, parameterTokenList: list of parameter expressions
    def __init__(self, nameToken, parameterNodeList):
        self.nameToken = nameToken
        self.parameterNodeList = parameterNodeList
    # String representation of FunctionCallNode
    def __repr__(self):
        return f'FUNCTION: {self.nameToken.value}; PARAMS: {self.parameterNodeList}'

# Represents a unary operation in the AST
class UnaryOpNode:
    # leftNode: left operand, opToken: token of operator, rightNode: rightOperand
    def __init__(self, opToken, rightNode):
        self.opToken = opToken
        self.rightNode = rightNode
    # String representation of a BinaryOpNode
    def __repr__(self):
        return f'({self.opToken}, {self.rightNode})'

# Represents a binary operation in the AST
class BinaryOpNode:
    # leftNode: left operand, opToken: token of operator, rightNode: rightOperand
    def __init__(self, leftNode, opToken, rightNode):
        self.leftNode = leftNode
        self.opToken = opToken
        self.rightNode = rightNode
    # String representation of a BinaryOpNode
    def __repr__(self):
        return f'({self.leftNode}, {self.opToken}, {self.rightNode})'

# Integer and Float parent class
class Number:
    # value: Value of number
    def __init__(self, value):
        self.value = value
    # String representation of Number
    def __repr__(self):
        return f'{self.value}'
    # Adds two numbers
    def addTo(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For +')
        if self.value + data.value != round(self.value + data.value):
            return Float(self.value + data.value)
        else:
            return Integer(self.value + data.value)
    # Subtracts two numbers
    def subtractBy(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For -')
        if self.value - data.value != round(self.value - data.value):
            return Float(self.value - data.value)
        else:
            return Integer(self.value - data.value)
    # Multiplies two numbers
    def multiplyBy(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For *')
        if self.value * data.value != round(self.value * data.value):
            return Float(self.value * data.value)
        else:
            return Integer(self.value * data.value)
    # Divides two numbers
    def divideBy(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For /')
        if data.value == 0:
            raise ZeroDivisionError('Cannot Divide By 0')
        return Float(self.value / data.value)
    # Raises one number to the power of the other
    def powerBy(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For ^')
        if self.value ** data.value != round(self.value ** data.value):
            return Float(self.value ** data.value)
        else:
            return Integer(self.value ** data.value)
    # Checks if argument is equal
    def equalTo(self, data):
        return Boolean(self.value == data.value)
    # Checks if argument is not equal to
    def notEqualTo(self, data):
        return Boolean(self.value != data.value)
    # Checks if self is lesser
    def lesserThan(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For <')
        return Boolean(self.value < data.value)
    # Checks if self is lesser or equal to
    def lesserEqualThan(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For <=')
        return Boolean(self.value <= data.value)
    # Checks if self is greater
    def greaterThan(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For >')
        return Boolean(self.value > data.value)
    # Checks if self is greater or equal to
    def greaterEqualThan(self, data):
        if not isinstance(data, Integer) and not isinstance(data, Float):
            raise TypeError(f'{type(self).__name__} & {type(data).__name__} Invalid Operands For >=')
        return Boolean(self.value >= data.value)
    # Negates number
    def negate(self):
        if type(self).__name__ == 'Integer':
            return Integer(self.value * -1)
        return Float(self.value * -1)
    # Returns a string version
    def toString(self):
        return String(str(self.value))
    # Returns an integer version
    def toInteger(self):
        try:
            return Integer(int(self.value))
        except:
            raise TypeError(f'Invalid {type(self).__name__} Conversion to Integer')
    # Returns a float version
    def toFloat(self):
        try:
            return Float(float(self.value))
        except:
            raise TypeError(f'Invalid {type(self).__name__} Conversion to Float')
    # Called on print()
    def printConsole(self):
        print(self.value, end = '')

# Elementary data type: Integer
class Integer(Number):
    # Passes initialisation to Number
    def __init__(self, value):
        super().__init__(value)
    # Mods two numbers
    def modBy(self, data):
        if not isinstance(data, Integer):
            raise TypeError(f'Integer & {type(data).__name__} Invalid Operands For MOD')
        if data.value == 0:
            raise ZeroDivisionError('Cannot MOD By 0')
        return Integer(self.value % data.value)
    # Integer divides two numbers
    def integerDivideBy(self, data):
        if not isinstance(data, Integer):
            raise TypeError(f'Integer & {type(data).__name__} Invalid Operands For DIV')
        if data.value == 0:
            raise ZeroDivisionError('Cannot DIV By 0')
        return Integer(self.value // data.value)

# Elementary data type: Float
class Float(Number):
    # Passes initialisation to Number
    def __init__(self, value):
        super().__init__(value)

# Elementary data type: String
class String:
    # value: Value of string
    def __init__(self, value):
        self.value = value
    # Concatenates two strings
    def addTo(self, data):
        if not isinstance(data, String):
            raise TypeError(f'String & {type(data).__name__} Invalid Operands For +')
        return String(self.value + data.value)
    # Repeats the string
    def multiplyBy(self, data):
        if not isinstance(data, Integer):
            raise TypeError(f'String & {type(data).__name__} Invalid Operands For *')
        return String(self.value * data.value)
    # Checks if argument is equal
    def equalTo(self, data):
        return Boolean(self.value == data.value)
    # Checks if argument is not equal to
    def notEqualTo(self, data):
        return Boolean(self.value != data.value)
    # Checks if self is lesser
    def lesserThan(self, data):
        if not isinstance(data, String):
            raise TypeError(f'String & {type(data).__name__} Invalid Operands For <')
        return Boolean(self.value < data.value)
    # Checks if self is lesser or equal to
    def lesserEqualThan(self, data):
        if not isinstance(data, String):
            raise TypeError(f'String & {type(data).__name__} Invalid Operands For <=')
        return Boolean(self.value <= data.value)
    # Checks if self is greater
    def greaterThan(self, data):
        if not isinstance(data, String):
            raise TypeError(f'String & {type(data).__name__} Invalid Operands For >')
        return Boolean(self.value > data.value)
    # Checks if self is greater or equal to
    def greaterEqualThan(self, data):
        if not isinstance(data, String):
            raise TypeError(f'String & {type(data).__name__} Invalid Operands For >=')
        return Boolean(self.value >= data.value)
    # Returns a string version
    def toString(self):
        return String(self.value)
    # Returns an integer version
    def toInteger(self):
        try:
            return Integer(int(self.value))
        except:
            raise TypeError('Invalid String Conversion to Integer')
    # Returns a float version
    def toFloat(self):
        try:
            return Float(float(self.value))
        except:
            raise TypeError('Invalid String Conversion to Float')
    # Returns a boolean version
    def toBool(self):
        try:
            return Boolean(bool(self.value))
        except:
            raise TypeError('Invalid String Conversion to Boolean')
    # Gets length
    def getLength(self):
        return Integer(len(self.value))
    # Gets character at given index
    def charAt(self, index):
        return String(self.value[index])
    # Called on print()
    def printConsole(self):
        print(self.value, end = '')

# Elementary data type: Boolean
class Boolean:
    # value: Value of boolean
    def __init__(self, value):
        self.value = value
    # Ands two boolean values
    def andWith(self, data):
        if not isinstance(data, Boolean):
            raise TypeError(f'Boolean & {type(data).__name__} Invalid Operands For AND')
        return Boolean(self.value and data.value)
    # Ors two boolean values
    def orWith(self, data):
        if not isinstance(data, Boolean):
            raise TypeError(f'Boolean & {type(data).__name__} Invalid Operands For OR')
        return Boolean(self.value or data.value)
    # Negates boolean value
    def negate(self):
        return Boolean(not self.value)
    # Checks if argument is equal
    def equalTo(self, data):
        return Boolean(self.value == data.value)
    # Checks if argument is not equal to
    def notEqualTo(self, data):
        return Boolean(self.value != data.value)
    # Returns a string version
    def toString(self):
        if self.value:
            return String('True')
        return String('False')
    # Returns a boolean version
    def toBool(self):
        return Boolean(bool(self.value))
    # Called on print()
    def printConsole(self):
        print(self.value, end = '')

# Function data type
class Function:
    # parameterNameTokenList: as before, statementList: as before
    def __init__(self, name, parameterNameTokenList, statementList):
        self.value = [name, parameterNameTokenList, statementList]
    # Checks if argument is equal
    def equalTo(self, data):
        return Boolean(self.value == data.value)
    # Checks if argument is not equal to
    def notEqualTo(self, data):
        return Boolean(self.value != data.value)
    # Called on print()
    def printConsole(self):
        print(f'Function: {self.value[0]}', end = '')

# Procedure data type
class Procedure:
    # parameterNameTokenList: as before, statementList: as before
    def __init__(self, name, parameterNameTokenList, statementList):
        self.value = [name, parameterNameTokenList, statementList]
    # Checks if argument is equal
    def equalTo(self, data):
        return Boolean(self.value == data.value)
    # Checks if argument is not equal to
    def notEqualTo(self, data):
        return Boolean(self.value != data.value)
    # Called on print()
    def printConsole(self):
        print(f'Procedure: {self.value[0]}', end = '')

# Interprets the AST recursively
class Interpreter:
    # symbolTable: Stores variables by scope
    def __init__(self, symbolTable = []):
        self.symbolTable = symbolTable
    # Checks if identifier exists and returns the correct value, else None
    def getIdentifierValue(self, name):
        for scope in self.symbolTable[::-1]:
            if name in scope:
                return scope[name]
        return None
    # Transfers visiting the node
    def visit(self, node):
        methodName = f'visit{type(node).__name__}'
        visitMethod = getattr(self, methodName, self.visitNotFound)
        return visitMethod(node)
    # Throws error if argument is undefined
    def visitNotFound(self, node):
        raise Exception(f'visit{type(node).__name__} Not Defined')
    # Visits ProgramNode
    def visitProgramNode(self, node):
        self.symbolTable.append({})
        for statementNode in node.statementList:
            retVal = self.visit(statementNode)
            if retVal is not None:
                raise Exception('Return Statement Cannot Exist Outside Of A Function')
        self.symbolTable.pop()
    # Visits VariableDeclarationStatementNode
    def visitVariableDeclarationStatementNode(self, node):
        variableName = node.identifierNode.nameToken.value
        if variableName in ['print']:
            raise SyntaxError(f'{variableName} Is A Keyword Procedure')
        elif variableName in ('input', 'int', 'float', 'str', 'bool'):
            raise SyntaxError(f'{variableName} Is A Keyword Function')
        variableValue = self.visit(node.expressionNode)
        self.symbolTable[-1][variableName] = variableValue
    # Visits FunctionDeclarationNode
    def visitFunctionDeclarationNode(self, node):
        functionName = node.nameToken.value
        if functionName in ['print']:
            raise SyntaxError(f'{functionName} Is A Keyword Procedure')
        elif functionName in ('input', 'int', 'float', 'str', 'bool'):
            raise SyntaxError(f'{functionName} Is A Keyword Function')
        parameterNameTokenList = node.parameterNameTokenList
        statementList = node.statementList
        self.symbolTable[-1][functionName] = Function(functionName, parameterNameTokenList, statementList)
    # Visits ProcedureDeclarationNode
    def visitProcedureDeclarationNode(self, node):
        procedureName = node.nameToken.value
        if procedureName in ['print']:
            raise SyntaxError(f'{procedureName} Is A Keyword Procedure')
        elif procedureName in ('input', 'int', 'float', 'str', 'bool'):
            raise SyntaxError(f'{procedureName} Is A Keyword Function')
        parameterNameTokenList = node.parameterNameTokenList
        statementList = node.statementList
        self.symbolTable[-1][procedureName] = Procedure(procedureName, parameterNameTokenList, statementList)
    # Visits ProcedureCallNode
    def visitProcedureCallNode(self, node):
        if node.nameToken.value == 'print':
            printString = ''
            for arg in node.parameterNodeList:
                printString += self.visit(arg).toString().value
            print(printString)
            return
        identifierValue = self.getIdentifierValue(node.nameToken.value)
        if identifierValue is None:
            raise NameError(f'No Identifier Named {node.nameToken.value}')
        if type(identifierValue).__name__ != 'Procedure':
            raise NameError('Identifier Used Does Not Belong To A Procedure Type')
        self.symbolTable.append({})
        parameterNodeList = node.parameterNodeList
        parameterNameTokenList = identifierValue.value[1]
        if len(parameterNodeList) != len(parameterNameTokenList):
            raise TypeError(f'{node.nameToken.value} Invalid Number of Arguments Passed')
        for i in range(len(parameterNameTokenList)):
            self.visitVariableDeclarationStatementNode(VariableDeclarationStatementNode(VariableNode(parameterNameTokenList[i]), parameterNodeList[i]))
        for statementNode in identifierValue.value[2]:
            retVal = self.visit(statementNode)
            if retVal is not None:
                raise Exception('Procedure Cannot Have A Return Statement')
        self.symbolTable.pop()
    # Visits FunctionCallNode
    def visitFunctionCallNode(self, node):
        if node.nameToken.value == 'input':
            if len(node.parameterNodeList) > 1:
                raise TypeError('input Expects 0/1 Argument(s)')
            if len(node.parameterNodeList) == 0:
                return String(input())
            prompt = self.visit(node.parameterNodeList[0])
            if type(prompt).__name__ != 'String':
                raise TypeError('input Expects A String Argument Type')
            return String(input(prompt.value))
        elif node.nameToken.value == 'int':
            if len(node.parameterNodeList) != 1:
                raise TypeError('int Expects Only 1 Argument')
            arg = self.visit(node.parameterNodeList[0])
            if type(arg).__name__ not in ('Integer', 'Float', 'String'):
                raise TypeError('int Expects Types Integer/Float/String')
            return arg.toInteger()
        elif node.nameToken.value == 'float':
            if len(node.parameterNodeList) != 1:
                raise TypeError('float Expects Only 1 Argument')
            arg = self.visit(node.parameterNodeList[0])
            if type(arg).__name__ not in ('Integer', 'Float', 'String'):
                raise TypeError('float Expects Types Integer/Float/String')
            return arg.toFloat()
        elif node.nameToken.value == 'str':
            if len(node.parameterNodeList) != 1:
                raise TypeError('str Expects Only 1 Argument')
            arg = self.visit(node.parameterNodeList[0])
            if type(arg).__name__ not in ('Integer', 'Float', 'String', 'Boolean'):
                raise TypeError('str Expects Types Integer/Float/String/Boolean')
            return arg.toString()
        elif node.nameToken.value == 'bool':
            if len(node.parameterNodeList) != 1:
                raise TypeError('bool Expects Only 1 Argument')
            arg = self.visit(node.parameterNodeList[0])
            if type(arg).__name__ not in ('String', 'Boolean'):
                raise TypeError('bool Expects Types String/Boolean')
            return arg.toBool()
        elif node.nameToken.value == 'len':
            if len(node.parameterNodeList) != 1:
                raise TypeError('len Expects Only 1 Argument')
            arg = self.visit(node.parameterNodeList[0])
            if type(arg).__name__ != 'String':
                raise TypeError('bool Expects Type String')
            return arg.getLength()
        elif node.nameToken.value == 'charAt':
            if len(node.parameterNodeList) != 2:
                raise TypeError('len Expects 2 Arguments')
            strArg = self.visit(node.parameterNodeList[0])
            idxArg = self.visit(node.parameterNodeList[1])
            if type(strArg).__name__ != 'String' or type(idxArg).__name__ != 'Integer':
                raise TypeError('Invalid Argument Types')
            return strArg.charAt(idxArg.value)
        identifierValue = self.getIdentifierValue(node.nameToken.value)
        if identifierValue is None:
            raise NameError(f'No Identifier Named {node.nameToken.value}')
        if type(identifierValue).__name__ != 'Function':
            raise NameError('Identifier Used Does Not Belong To A Function Type')
        self.symbolTable.append({})
        parameterNodeList = node.parameterNodeList
        parameterNameTokenList = identifierValue.value[1]
        if len(parameterNodeList) != len(parameterNameTokenList):
            raise TypeError(f'{node.nameToken.value} Invalid Number of Arguments Passed')
        for i in range(len(parameterNameTokenList)):
            self.visitVariableDeclarationStatementNode(VariableDeclarationStatementNode(VariableNode(parameterNameTokenList[i]), parameterNodeList[i]))
        for statementNode in identifierValue.value[2]:
            retVal = self.visit(statementNode)
            if retVal is not None:
                self.symbolTable.pop()
                return retVal
        raise Exception(f'{node.nameToken.value} Function Does Not Contain Return Node')
    # Visits ReturnNode
    def visitReturnNode(self, node):
        return self.visit(node.returnExpression)
    # Visits IfStatementNode
    def visitIfStatementNode(self, node):
        for branch in node.branchList:
            if self.visit(branch.condition).value:
                return self.visit(branch)
    # Visits BranchNode
    def visitBranchNode(self, node):
        for statementNode in node.statementList:
            retVal = self.visit(statementNode)
            if retVal is not None:
                return retVal
    # Visits SwitchStatementNode
    def visitSwitchStatementNode(self, node):
        for branch in node.branchList:
            if self.visit(branch.condition).value:
                return self.visit(branch)
    # Visits WhileStatementNode
    def visitWhileStatementNode(self, node):
        while self.visit(node.condition).value:
            for statementNode in node.statementList:
                retVal = self.visit(statementNode)
                if retVal is not None:
                    return retVal
    # Visits ForStatementNode
    def visitForStatementNode(self, node):
        differenceBool = self.visitBinaryOpNode(BinaryOpNode(node.startNode, Token(TT_LESSEREQUALITY), node.endNode))
        endVal = self.visit(node.endNode)
        self.visitVariableDeclarationStatementNode(VariableDeclarationStatementNode(VariableNode(node.iterNameToken), node.startNode))
        if differenceBool.value:
            while self.getIdentifierValue(node.iterNameToken.value).lesserEqualThan(endVal).value:
                for statementNode in node.statementList:
                    retVal = self.visit(statementNode)
                    if retVal is not None:
                        return retVal
                self.symbolTable[-1][node.iterNameToken.value] = self.symbolTable[-1][node.iterNameToken.value].addTo(Integer(1))
        else:
            while self.getIdentifierValue(node.iterNameToken.value).greaterEqualThan(endVal).value:
                for statementNode in node.statementList:
                    retVal = self.visit(statementNode)
                    if retVal is not None:
                        return retVal
                self.symbolTable[-1][node.iterNameToken.value] = self.symbolTable[-1][node.iterNameToken.value].subtractBy(Integer(1))
    # Visits DoUntilStatementNode
    def visitDoUntilStatementNode(self, node):
        for statementNode in node.statementList:
            retVal = self.visit(statementNode)
            if retVal is not None:
                return retVal
        while not self.visit(node.condition).value:
            for statementNode in node.statementList:
                retVal = self.visit(statementNode)
                if retVal is not None:
                    return retVal
    # Visits BinaryOpNode
    def visitBinaryOpNode(self, node):
        leftVal = self.visit(node.leftNode)
        operator = node.opToken.type
        rightVal = self.visit(node.rightNode)
        if operator == TT_PLUS:
            if type(leftVal).__name__ not in ('String', 'Integer', 'Float'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For +')
            return leftVal.addTo(rightVal)
        elif operator == TT_MINUS:
            if type(leftVal).__name__ not in ('Integer', 'Float'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For -')
            return leftVal.subtractBy(rightVal)
        elif operator == TT_MULTIPLY:
            if type(leftVal).__name__ not in ('String', 'Integer', 'Float'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For *')
            return leftVal.multiplyBy(rightVal)
        elif operator == TT_DIVIDE:
            if type(leftVal).__name__ not in ('Integer', 'Float'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For /')
            return leftVal.divideBy(rightVal)
        elif operator == TT_MOD:
            if type(leftVal).__name__ != 'Integer':
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For MOD')
            return leftVal.modBy(rightVal)
        elif operator == TT_DIV:
            if type(leftVal).__name__ != 'Integer':
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For DIV')
            return leftVal.integerDivideBy(rightVal)
        elif operator == TT_POWER:
            if type(leftVal).__name__ not in ('Integer', 'Float'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For ^')
            return leftVal.powerBy(rightVal)
        elif operator == TT_AND:
            if type(leftVal).__name__ != 'Boolean':
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For AND')
            return leftVal.andWith(rightVal)
        elif operator == TT_OR:
            if type(leftVal).__name__ != 'Boolean':
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For OR')
            return leftVal.orWith(rightVal)
        elif operator == TT_EQUALITY:
            return leftVal.equalTo(rightVal)
        elif operator == TT_NONEQUALITY:
            return leftVal.notEqualTo(rightVal)
        elif operator == TT_LESSER:
            if type(leftVal).__name__ not in ('Integer', 'Float', 'String'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For <')
            return leftVal.lesserThan(rightVal)
        elif operator == TT_LESSEREQUALITY:
            if type(leftVal).__name__ not in ('Integer', 'Float', 'String'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For <=')
            return leftVal.lesserEqualThan(rightVal)
        elif operator == TT_GREATER:
            if type(leftVal).__name__ not in ('Integer', 'Float', 'String'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For >')
            return leftVal.greaterThan(rightVal)
        elif operator == TT_GREATEREQUALITY:
            if type(leftVal).__name__ not in ('Integer', 'Float', 'String'):
                raise TypeError(f'{type(leftVal).__name__} Invalid Operand For >=')
            return leftVal.greaterEqualThan(rightVal)
        else:
            raise Exception('Undetermined Error')
    # Visits UnaryOpNode
    def visitUnaryOpNode(self, node):
        operator = node.opToken.type
        operand = self.visit(node.rightNode)
        if operator == TT_MINUS:
            if type(operand).__name__ not in ('Integer', 'Float'):
                raise TypeError(f'{type(operand).__name__} Invalid Operand For - Negation')
            return operand.negate()
        elif operand == TT_NOT:
            if type(operand).__name__ != 'Boolean':
                raise TypeError(f'{type(operand).__name__} Invalid Operand For Not Negation')
            return operand.negate()
        else:
            raise Exception('Undetermined Error')
    # Visits NumberLiteralNode
    def visitNumberLiteralNode(self, node):
        numValue = node.numToken.value
        numType = node.numToken.type
        if numType == TT_INTEGERLITERAL:
            return Integer(numValue)
        return Float(numType)
    # Visits StringLiteralNode
    def visitStringLiteralNode(self, node):
        return String(node.strToken.value)
    # Visits BooleanLiteralNode
    def visitBooleanLiteralNode(self, node):
        return Boolean(node.boolToken.value)
    # Visits VariableNode
    def visitVariableNode(self, node):
        variableName = node.nameToken.value
        variableValue = self.getIdentifierValue(variableName)
        if variableValue is None:
            raise NameError(f'No Identifier Named {node.nameToken.value}')
        return variableValue

# TEMPORARY Testing
Interpreter().visit(Parser(Lexer('test.txt').getTokens()).parseProgram())
