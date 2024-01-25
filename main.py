from genereTreeGraphviz2 import print_tree_graph

reserved = {
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'print': 'PRINT',
    'printString': 'PRINTSTRING',
    'function': 'FUNCTION'
}

tokens = [
             'NAME', 'NUMBER', 'STRING',
             'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
             'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'COLON', 'COMMA', 'QUOTE',
             'AND', 'OR', 'EQUAL', 'EQUALS', 'LOWER', 'HIGHER', 'COMMENT'
         ] + list(reserved.values())


# Tokens
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'NAME')  # Check for reserved words
    return t


t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUAL = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\{'
t_RBRACKET = r'\}'
t_COLON = r';'
t_COMMA = r','
t_QUOTE = r'"'
t_AND = r'\&'
t_OR = r'\|'
t_EQUALS = r'=='
t_LOWER = r'\<'
t_HIGHER = r'\>'
t_STRING = r'"[^"]+"'
t_COMMENT = r'\/\*[^\*\/]*\*\/'


def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t


# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
import ply.lex as lex

lexer = lex.lex()


# Parsing rules


def p_start(t):
    ''' start : linst'''
    t[0] = ('start', t[1])
    print('tree =', t[0])
    print_tree_graph(t[0])
    # eval(t[1])
    eval_inst(t[1])


names = {}
functions = {}


def eval_inst(t):
    # print('evalInst de ', t)
    if t == 'empty':
        return
    if type(t) is not tuple:
        print('Warning; type =', type(t), '; t =', t, ';')
        return
    if t[0] == 'print':
        print('CALC>', eval_expr(t[1]))
    if t[0] == 'print_string':
        print('STRING>', t[1][1:-1])
    if t[0] == 'assign':
        names[t[1]] = eval_expr(t[2])
    if t[0] == 'short_assign':
        if t[2] == '++':
            names[t[1]] = names[t[1]] + 1
        if t[2] == '--':
            names[t[1]] = names[t[1]] - 1
    if t[0] == 'op_assign':
        if t[2] == '+':
            names[t[1]] += t[3]
        if t[2] == '-':
            names[t[1]] -= t[3]
        if t[2] == '*':
            names[t[1]] *= t[3]
        if t[2] == '/':
            names[t[1]] /= t[3]
    if t[0] == 'if' or t[0] == 'elif':
        if eval_expr(t[1]):
            eval_inst(t[2])
        elif t[3] != ';':
            eval_inst(t[3])
    if t[0] == 'else':
        eval_inst(t[1])
    if t[0] == 'while':
        while eval_expr(t[1]):
            eval_inst(t[2])
    if t[0] == 'for':
        eval_inst(t[1])
        while eval_expr(t[2]):
            if not eval_expr(t[2]):
                break
            eval_inst(t[4])
            eval_inst(t[3])
    if t[0] == 'define_function':
        functions[t[1]] = t[2]
    if t[0] == 'call_function':
        eval_inst(functions[t[1]])
    if t[0] == 'bloc':
        eval_inst(t[1])
        eval_inst(t[2])


def eval_expr(t):
    # print('evalExpr de ', t)
    if type(t) is int:
        return t
    if type(t) is str:
        if t in names:
            return names[t]
        print("Unknown variable '%s'" % t)
    if type(t) is tuple:
        if t[0] == '+':
            return eval_expr(t[1]) + eval_expr(t[2])
        if t[0] == '-':
            return eval_expr(t[1]) - eval_expr(t[2])
        if t[0] == '*':
            return eval_expr(t[1]) * eval_expr(t[2])
        if t[0] == '/':
            return eval_expr(t[1]) // eval_expr(t[2])
        if t[0] == '&':
            return eval_expr(t[1]) and eval_expr(t[2])
        if t[0] == '|':
            return eval_expr(t[1]) or eval_expr(t[2])
        if t[0] == '==':
            return eval_expr(t[1]) == eval_expr(t[2])
        if t[0] == '<':
            return eval_expr(t[1]) < eval_expr(t[2])
        if t[0] == '>':
            return eval_expr(t[1]) > eval_expr(t[2])
    print("Unknown expression '%s'" % t[0])


def p_line(t):
    '''linst : linst inst 
            | inst '''
    if len(t) == 3:
        t[0] = ('bloc', t[1], t[2])
    else:
        t[0] = ('bloc', t[1], 'empty')


def p_statement_assign(t):
    'inst : NAME EQUAL expression COLON'
    t[0] = ('assign', t[1], t[3])


def p_statement_short_assign(t):
    '''inst : NAME PLUS PLUS COLON
            | NAME MINUS MINUS COLON'''
    t[0] = ('short_assign', t[1], t[2] + t[3])


def p_statement_op_assign(t):
    '''inst : NAME PLUS EQUAL NUMBER COLON
            | NAME MINUS EQUAL NUMBER COLON
            | NAME TIMES EQUAL NUMBER COLON
            | NAME DIVIDE EQUAL NUMBER COLON'''
    t[0] = ('op_assign', t[1], t[2], t[4])


def p_statement_print(t):
    'inst : PRINT LPAREN expression RPAREN COLON'
    t[0] = ('print', t[3])


def p_statement_print_string(t):
    'inst : PRINTSTRING LPAREN STRING RPAREN COLON'
    t[0] = ('print_string', t[3])


def p_statement_elif(t):
    '''lif : ELIF LPAREN expression RPAREN LBRACKET linst RBRACKET lif
            | ELIF LPAREN expression RPAREN LBRACKET linst RBRACKET COLON
            | ELSE LBRACKET linst RBRACKET COLON'''
    if t[1] == 'elif':
        t[0] = ('elif', t[3], t[6], t[8])
    else:
        t[0] = ('else', t[3])


def p_statement_lif(t):
    '''inst : IF LPAREN expression RPAREN LBRACKET linst RBRACKET COLON
            | IF LPAREN expression RPAREN LBRACKET linst RBRACKET lif'''
    t[0] = ('if', t[3], t[6], t[8])


def p_statement_while(t):
    'inst : WHILE LPAREN expression RPAREN LBRACKET linst RBRACKET COLON'
    t[0] = ('while', t[3], t[6])


def p_statement_for(t):
    'inst : FOR LPAREN inst expression COLON inst RPAREN LBRACKET linst RBRACKET COLON'
    t[0] = ('for', t[3], t[4], t[6], t[9])


def p_statement_define_function(t):
    'inst : FUNCTION NAME LPAREN RPAREN LBRACKET linst RBRACKET COLON'
    t[0] = ('define_function', t[2], t[6])


def p_statement_call_function(t):
    'inst : NAME LPAREN RPAREN COLON'
    t[0] = ('call_function', t[1])


def p_statement_comment(t):
    'inst : COMMENT'
    t[0] = ('comment', t[1])


def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression OR expression
                  | expression AND expression
                  | expression EQUALS expression
                  | expression LOWER expression
                  | expression HIGHER expression
                  | expression DIVIDE expression'''
    t[0] = (t[2], t[1], t[3])


def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]


def p_expression_number(t):
    'expression : NUMBER'
    t[0] = t[1]


def p_expression_name(t):
    'expression : NAME'
    t[0] = t[1]


def p_error(t):
    print("Syntax error at '%s'" % t.value)


import ply.yacc as yacc

parser = yacc.yacc()

with open("inputFiles/input.in") as file:  # Use file to refer to the file object
    s = file.read()

parser.parse(s)
