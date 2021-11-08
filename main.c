#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

typedef unsigned char vectorBytes_t[5000];
typedef char string[500];
typedef enum
{
    CONST, IDENT, EQUAL, PROCEDURE, GREATER, LESS, EOFILE, NUMBER, NOTHING, VAR,
    COMMA, SEMICOLON, IF, THEN, BEGIN, CALL, END, ASSIGNMENT, MULTIPLY, OPENPARENTHESES,
    CLOSEPARENTHESES, POINT, READLN, WRITELN, WRITE, WHILE, DIVIDE,
    DO, STRING, ODD, DISTINCT, GREATEROREQUAL, LESSOREQUAL, PLUS, MINUS, SQR
} symbol_t;

typedef struct {
    string name;
    symbol_t type;
    int value;

} ident_t;

typedef ident_t identifierTable[500];

void program(FILE*, FILE*, string, vectorBytes_t, int*, string, string, symbol_t*, int*, identifierTable);
void block(FILE*, FILE*, string, vectorBytes_t, int*, int*, string, string, symbol_t*, int*, int, identifierTable);
void proposition (FILE*, FILE*, string, vectorBytes_t, int*, string, string, symbol_t*, int*, int, int, identifierTable);
void condition(FILE*, FILE*, string, vectorBytes_t, int*, string, string, symbol_t*, int*, int, int, identifierTable);
void expression(FILE*, FILE*, string, vectorBytes_t, int*, string, string, symbol_t*, int*, int, int, identifierTable);
void term(FILE*, FILE*, string, vectorBytes_t, int*, string, string, symbol_t*, int*, int, int, identifierTable);
void factor(FILE*, FILE*, string, vectorBytes_t, int*, string, string, symbol_t*, int*, int, int, identifierTable);

void checkResultCompilation(FILE* file, FILE* errorFile, string errorFileName, int code) {
    if(0 == code) {
        fclose(errorFile);
    } else {
        printf("ERROR!!\n Se ha generado un archivo llamado %s para ayudarle a identificar el problema.", errorFileName);
    }
    fclose(errorFile);
    fclose(file);
    exit(0);
}

void generateByte(unsigned char data, vectorBytes_t memory, int* memoryLimit) {
    memory[(*memoryLimit)++] = data;
}

void generateInt(int data, vectorBytes_t memory, int* memoryLimit) {
    unsigned char b1 = data>>24; // Most significant.
    unsigned char b2 = (data<<8)>>24;
    unsigned char b3 = (data<<16)>>24;
    unsigned char b4 = (data<<24)>>24; //Most significant.

    // mnemónico abcdefgh = 1234
    // 1 0000 0010 (258)
    // 0 1111 1111 (255)
    // 0 0000 0010 (258 AND 255)
    // bytes ghefcdab = 4321

    // 0000 0000 0000 0000 0000 0001 0000 0010 (258 en 32bits)
    // Moves bits 24 to the left.
    // 0000 0010 0000 0000 0000 0000 0000 0000
    // Moves bits 24 to the right.
    // 0000 0000 0000 0000 0000 0000 0000 0010 (2)

    // 0000 0000 0000 0000 0000 0001 0000 0010 (258 en 32bits)
    // Moves bits 16 to the left.
    // 0000 0001 0000 0010 0000 0000 0000 0000
    // Moves bits 24 to the right.
    // 0000 0000 0000 0000 0000 0000 0000 0001

    memory[(*memoryLimit)++] = b4;
    memory[(*memoryLimit)++] = b3;
    memory[(*memoryLimit)++] = b2;
    memory[(*memoryLimit)++] = b1;
}

void generateIntIn(int data, int position, vectorBytes_t memory) {
    unsigned char b1 = data>>24; // Most significant.
    unsigned char b2 = (data<<8)>>24;
    unsigned char b3 = (data<<16)>>24;
    unsigned char b4 = (data<<24)>>24; // Less significant

    memory[position] = b4;
    memory[position+1] = b3;
    memory[position+2] = b2;
    memory[position+3] = b1;
}

int readIntSince (int position, vectorBytes_t memory) {
    return memory[position] + (memory[position+1]<<8) + 
           (memory[position+2]<<16) + (memory[position+3]<<24);
}

void stringToUppercase(string str) {
    for (int i = 0; i < strlen(str); i++) {
        str[i] = toupper(str[i]);
    }
}

void reducer(string str) {
    int i;
    for (i = 0; i < strlen(str) - 1; i++) {
        str[i] = str[i + 1];
    }
    str[i] = '\0';
}

void setKeyWord(string str, symbol_t* symbol) {
    if (strcmp(str, "CONST") == 0) {
            *symbol = CONST;
        }
    else if (strcmp(str, "VAR") == 0) {
        *symbol = VAR;
    }
    else if (strcmp(str, "PROCEDURE") == 0) {
        *symbol = PROCEDURE;
    }
    else if (strcmp(str, "IF") == 0) {
        *symbol = IF;
    }
    else if (strcmp(str, "THEN") == 0) {
        *symbol = THEN;
    }
    else if (strcmp(str, "BEGIN") == 0) {
        *symbol = BEGIN;
    }
    else if (strcmp(str, "CALL") == 0) {
        *symbol = CALL;
    }
    else if (strcmp(str, "END") == 0) {
        *symbol = END;
    }
    else if (strcmp(str, "READLN") == 0) {
        *symbol = READLN;
    }
    else if (strcmp(str, "WRITELN") == 0) {
        *symbol = WRITELN;
    }
    else if (strcmp(str, "WRITE") == 0) {
        *symbol = WRITE;
    }
    else if (strcmp(str, "WHILE") == 0) {
        *symbol = WHILE;
    }
    else if (strcmp(str, "DO") == 0) {
        *symbol = DO;
    }
    else if (strcmp(str, "ODD") == 0) {
        *symbol = ODD;
    }
    else if (strcmp(str, "SQR") == 0) {
        *symbol = SQR;
    }
    else {
        *symbol = IDENT;
    }
}

void setSign(string str, string remaining, symbol_t* symbol, int i) {
    switch (remaining[0]) {
        case ':':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            i++;
            if(remaining[0] == '=') {
                *symbol = ASSIGNMENT;
                str[i] = remaining[0];
                str[i+1] = '\0';
                reducer(remaining);
                i++;
            } else {
                *symbol = NOTHING;
            }
            break;
        case '=':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = EQUAL;
            i++;
            break;
        case '>':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            i++;
            if (remaining[0] == '='){
                *symbol = GREATEROREQUAL;
                str[i] = remaining[0];
                str[i+1] = '\0';
                reducer(remaining);
                i++;
            } else {
                *symbol = GREATER;
            }
            break;
        case '<':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            i++;
            if (remaining[0] == '=') {
                str[i] = remaining[0];        
                str[i+1] = '\0';
                reducer(remaining);
                *symbol = LESSOREQUAL;
                i++;
            } else if (remaining[0] == '>') {
                str[i] = remaining[0];
                str[i+1] = '\0';
                reducer(remaining);
                *symbol = DISTINCT;
                i++;
            } else {
                *symbol = LESS;
            }
            break;
        case ';':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = SEMICOLON;
            i++;
            break;
        case ',':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = COMMA;
            i++;
            break;
        case '.':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = POINT;
            i++;
            break;
        case '\'':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            i++;
            while (remaining[0] != '\'' && remaining[0] != '\0'){
                str[i] = remaining[0];
                str[i+1] = '\0';
                reducer(remaining);
                i++;
            }
            if (remaining[0] == '\''){
                str[i] = remaining[0];
                str[i+1] = '\0';
                reducer(remaining);
                *symbol = STRING;
                i++;
            } else {
                *symbol = NOTHING;
            }
            break;
        case '+':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = PLUS;
            i++;
            break;
        case '-':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = MINUS;
            i++;
            break;
        case '*':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = MULTIPLY;
            i++;
            break;
        case '/':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = DIVIDE;
            i++;
            break;
        case '(':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = OPENPARENTHESES;
            i++;
            break;
        case ')':
            str[i] = remaining[0];
            str[i+1] = '\0';
            reducer(remaining);
            *symbol = CLOSEPARENTHESES;
            i++;
            break;
        default:
            str[i] = remaining[0];
            str[i + 1] = '\0';
            reducer(remaining);
            *symbol = NOTHING;
            i++;
            break;
        }
}

void setNumber(symbol_t* symbol) {
    *symbol = NUMBER;
}

void printSymbol(symbol_t *symbol) {
    switch (*symbol) {
    case CONST:
        printf("symbol_t: CONST\n");
        break;
    case VAR:
        printf("symbol_t: VAR\n");
        break;
    case IDENT:
        printf("symbol_t: IDENT\n");
        break;
    case COMMA:
        printf("symbol_t: COMMA\n");
        break;
    case SEMICOLON:
        printf("symbol_t: SEMICOLON\n");
        break;
    case PROCEDURE:
        printf("symbol_t: PROCEDURE\n");
        break;
    case GREATER:
        printf("symbol_t: GREATER\n");
        break;
    case LESS:
        printf("symbol_t: LESS\n");
        break;
    case IF:
        printf("symbol_t: IF\n");
        break;
    case THEN:
        printf("symbol_t: THEN\n");
        break;
    case BEGIN:
        printf("symbol_t: BEGIN\n");
        break;
    case CALL:
        printf("symbol_t: CALL\n");
        break;
    case END:
        printf("symbol_t: END\n");
        break;
    case NUMBER:
        printf("symbol_t: NUMBER\n");
        break;
    case ASSIGNMENT:
        printf("symbol_t: ASSIGNMENT\n");
        break;
    case MULTIPLY:
        printf("symbol_t: MULTIPLY\n");
        break;
    case OPENPARENTHESES:
        printf("symbol_t: OPENPARENTHESES\n");
        break;
    case CLOSEPARENTHESES:
        printf("symbol_t: CLOSEPARENTHESES\n");
        break;
    case PLUS:
        printf("symbol_t: PLUS\n");
        break;
    case MINUS:
        printf("symbol_t: MINUS\n");
        break;
    case POINT:
        printf("symbol_t: POINT\n");
        break;
    case READLN:
        printf("symbol_t: READLN\n");
        break;
    case WRITELN:
        printf("symbol_t: WRITELN\n");
        break;
    case WRITE:
        printf("symbol_t: WRITE\n");
        break;
    case STRING:
        printf("symbol_t: STRING\n");
        break;
    case WHILE:
        printf("symbol_t: WHILE\n");
        break;
    case DO:
        printf("symbol_t: DO\n");
        break;
    case SQR:
        printf("symbol_t: SQR\n");
        break;
    case EOFILE:
        printf("symbol_t: EOFILE\n");
        break;
    }
}

void getSymbol(FILE* file, FILE* errorFile, string str, string remaining, symbol_t* symbol, int* lineCount) {
    
    str[0] = '\0';
    int i = 0;
    while (remaining != NULL && strcmp(remaining, "") != 0 && isspace(remaining[0])) {
        reducer(remaining);
        if (strcmp(remaining, "") == 0) {
            fgets(remaining, 500, file);
            if (remaining != NULL) {
                fprintf(errorFile, "%3d: %s", *lineCount, remaining);
                (*lineCount)++;
            }
        }
    }
    if (remaining == NULL || strcmp(remaining, "") == 0 || isspace(remaining[0])){
        *symbol = EOFILE;
    }
    else if (isalpha(remaining[0])) {
        str[i] = remaining[0];
        str[i + 1] = '\0';
        reducer(remaining);
        i++;
        while (isalnum(remaining[0])) {
            str[i] = remaining[0];
            str[i + 1] = '\0';
            reducer(remaining);
            i++;
        }
        stringToUppercase(str);
        setKeyWord(str, symbol);
    }
    
    else if (isdigit(remaining[0])) {
        str[i] = remaining[0];
        str[i + 1] = '\0';
        reducer(remaining);
        i++;
        while (isdigit(remaining[0])) {
            str[i] = remaining[0];
            str[i + 1] = '\0';
            reducer(remaining);
            i++;
        }
        setNumber(symbol);  
    }
    else {
        setSign(str, remaining, symbol, i); 
    }
}

void showErrorCode(FILE* file, FILE* errorFile, string errorFileName, int code, string str){
    printf("\n");
    switch (code) {
    case 0:
        printf("Compilacion finalizada exitosamente.\n");
        break;
    case 1:
        fprintf(errorFile, "\nERROR: Hay caracteres despues del programa.");
        break;
    case 2:
        fprintf(errorFile, "\nERROR: Se esperaba un punto. Se leyo: %s", str);
        break;
    case 3:
        fprintf(errorFile, "\nERROR: Se esperaba un identificador. Se leyo: %s", str);
        break;
    case 4:
        fprintf(errorFile, "\nERROR: Se esperaba un igual. Se leyo: %s", str);
        break;
    case 5:
        fprintf(errorFile, "\nERROR: Se esperaba un numero. Se leyo: %s", str);
        break;
    case 6:
        fprintf(errorFile, "\nERROR: Se esperaba un punto y coma, o coma. Se leyo: %s", str);
        break;
    case 7:
        fprintf(errorFile, "\nERROR: Se esperaba una asignacion. Se leyo: %s", str);
        break;
    case 8:
        fprintf(errorFile, "\n: %sERROR: Se esperaba un operador relacional. Se leyo: %s", str);
        break;
    case 9:
        fprintf(errorFile, "\n: %sERROR: Se esperaba un identificador, numero, cadena o apertura de parentesis. Se leyo: %s" , str);
        break;
    case 10:
        fprintf(errorFile, "\n: %sERROR: Se esperaba un cierre de parentesis. Se leyo: %s", str);
        break;
    case 11:
        fprintf(errorFile, "\n: %sERROR: Se esperaba un punto y coma. Se leyo: %s", str);
        break;
    case 12:
        fprintf(errorFile, "\nERROR: Se esperaba un punto y coma o end. Se leyo: %s", str);
        break;
    case 13:
        fprintf(errorFile, "\nERROR: Se esperaba un THEN. Se leyo: %s", str);
        break;
    case 14:
        fprintf(errorFile, "\nERROR: Se esperaba un DO. Se leyo: %s", str);
        break;
    case 15:
        fprintf(errorFile, "\nERROR: Se esperaba una apertura de parentesis. Se leyo: %s", str);
        break;
    case 16:
        fprintf(errorFile, "\nERROR: Se esperaba un cierre de parentesis, o una coma. Se leyo: %s", str);
        break;
    case 17:
        fprintf(errorFile, "\nERROR: Identificador duplicado: %s", str);
        break;
    case 18:
        fprintf(errorFile, "\nERROR: Identificador no declarado: %s", str);
        break;
    case 19:
        fprintf(errorFile, "\nERROR: Se esperaba una constante o variable: %s", str);
        break;
    case 20:
        fprintf(errorFile, "\nERROR: El identificador es demasiado largo(la máxima logitud es 63): %s", str);
        break;
    case 21:
        fprintf(errorFile, "ERROR: Se esperaba una variable: %s", str);
        break;
    case 22:
        fprintf(errorFile, "ERROR: Se esperaba un posible numero negativo y se leyo: %s", str);
        break;
    }
    checkResultCompilation(file, errorFile, errorFileName, code);
}

int search(identifierTable table, int base, int limit, string identifierName){
    //printf("nombre: %s, base: %d, tope: %d\n", identifierName, base, limit);
    int position = base + limit - 1 ;
    while (position >= base && strcmp(table[position].name, identifierName) != 0 ){
        position--;
    }
    if (position == base - 1){
        return -1;
    } else {
        return position;
    }
}

void block(FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit,
           int *variablesCounter, string str, string remaining, symbol_t* symbol, int* lineCount,
           int base, identifierTable table){
    
    int displacement = 0;
    const int integerSizeInBytes = 4; // 4 = 32 bits, 8 = 64 bits...
    generateByte(0xE9, memory, memoryLimit); // JMP
    generateInt(0, memory, memoryLimit);
    int startBlock = *memoryLimit;

    if (*symbol == CONST) {
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        if (*symbol == IDENT) {
            if (search(table, base, base + displacement, str) != -1) {
                showErrorCode(file, errorFile, errorFileName, 17, str);
            } else {
                strcpy(table[base + displacement].name, str);
                table[base + displacement].type = CONST;
            }
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 3, str);
        }
        if (*symbol == EQUAL) {
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 4, str);
        }
        if (*symbol == NUMBER) {
            table[base + displacement].value = atoi(str);
            displacement++;
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        } else if (*symbol == MINUS) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                if(*symbol == NUMBER) {
                   table[base + displacement].value = - atoi(str);
                   displacement++;
                   getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                } else {
                     showErrorCode(file, errorFile, errorFileName, 22, str);
                }
        } else {
            showErrorCode(file, errorFile, errorFileName, 5, str);
        } 
        while (*symbol == COMMA) {
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            if (*symbol == IDENT) {
                if (search(table, base, base + displacement, str) != -1) {
                    showErrorCode(file, errorFile, errorFileName, 17, str);
                } else {
                    strcpy(table[base + displacement].name, str);
                    table[base + displacement].type = CONST;
                }
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 20, str);
            }
            if (*symbol == EQUAL) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 4, str);
            }
            if (*symbol == NUMBER) {
                table[base + displacement].value = atoi(str);
                displacement++;
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            } else if (*symbol == MINUS) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                if(*symbol == NUMBER) {
                   table[base + displacement].value = - atoi(str);
                   displacement++;
                   getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                } else {
                     showErrorCode(file, errorFile, errorFileName, 22, str);
                }
            } else {
                showErrorCode(file, errorFile, errorFileName, 5, str);
            }
    
        }
        if (*symbol == SEMICOLON) {
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 6, str);
        }
    }
    if (*symbol == VAR) {
        getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
        if (*symbol == IDENT) {
            if (search(table, base, base + displacement, str) != -1) {
                    showErrorCode(file, errorFile, errorFileName, 17, str);
            } else {
                strcpy(table[base + displacement].name, str);
                table[base + displacement].type = VAR;
                table[base + displacement].value = integerSizeInBytes * (*variablesCounter);
                (*variablesCounter)++;
                displacement++;
            }
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 3, str);
        }
        while (*symbol == COMMA) {
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            if(strlen(str) <= 63) {
                if (*symbol == IDENT) {
                    if (search(table, base, base + displacement, str) != -1) {
                        showErrorCode(file, errorFile, errorFileName, 17, str);
                    } else {
                        strcpy(table[base + displacement].name, str);
                        table[base + displacement].type = VAR;
                        table[base + displacement].value = integerSizeInBytes * (*variablesCounter);
                        (*variablesCounter)++;
                        displacement++;
                    }
                    getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
                } else {
                    showErrorCode(file, errorFile, errorFileName, 3, str);
                }
            } else {
                showErrorCode(file, errorFile, errorFileName, 20, str);
            }
        }
        if (*symbol == SEMICOLON) {
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 6, str);
        }
    }
    while (*symbol == PROCEDURE) {
        getSymbol(file, errorFile, str, remaining,  symbol,  lineCount);
        if (*symbol == IDENT) {
            if (search(table, base, base + displacement, str) != -1) {
                showErrorCode(file, errorFile, errorFileName, 17, str);
            } else {
                strcpy(table[base + displacement].name, str);
                table[base + displacement].type = PROCEDURE;
                table[base + displacement].value = *memoryLimit;
                displacement++;
            }
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 3, str);
        }
        if (*symbol == SEMICOLON) {
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 11, str);
        }
        block(file, errorFile, errorFileName, memory, memoryLimit, variablesCounter, str, remaining, symbol,  lineCount, base + displacement, table);
        if (*symbol == SEMICOLON) {
            generateByte(0xC3, memory, memoryLimit); // RET
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 11, str);
        }
    }
    int distance = *memoryLimit - startBlock;
    generateIntIn(distance, startBlock - 4, memory);
    proposition(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol,  lineCount, base, displacement, table);
}

void proposition(FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit,
                 string str, string remaining, symbol_t* symbol, int* lineCount,
                 int base, int displacement, identifierTable table) {
    
    int positionIdent = 0;
    int distanceToRoutine;
    int jumpPosition;
    int jumpDistance;
    int cyclePosition;
    int cycleDistance;
    int startStr;

    switch(*symbol) {
        case IDENT:
            positionIdent = search(table, 0, base + displacement, str);
            if(-1 == positionIdent) {
                showErrorCode(file, errorFile, errorFileName, 18, str);
            } else if (table[positionIdent].type != VAR) {
                showErrorCode(file, errorFile, errorFileName, 21, str);
            }
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            if (*symbol == ASSIGNMENT){
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 7, str);
            }
            expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol,  lineCount, base, displacement, table);
            generateByte(0x58, memory, memoryLimit); // POP EAX.
            generateByte(0x89, memory, memoryLimit); // MOV [EDI+abcdefgh], EAX.
            generateByte(0x87, memory, memoryLimit);
            generateInt(table[positionIdent].value, memory, memoryLimit);
            break;
        case CALL:
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            if (*symbol == IDENT){
                 positionIdent = search(table, 0, base + displacement, str);
                if ( -1 == positionIdent) {
                    showErrorCode(file, errorFile, errorFileName, 18, str);
                } else if (table[positionIdent].type != PROCEDURE) {
                    showErrorCode(file, errorFile, errorFileName, 20, str);
                }
                distanceToRoutine = table[positionIdent].value - (*memoryLimit + 5) ; // Distance to procedure
                generateByte(0xE8, memory, memoryLimit); // CALL
                generateInt(distanceToRoutine, memory, memoryLimit); // distancia desde pos actual a la rutina
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 3, str);
            }
            break;
        case BEGIN:
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            proposition (file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
            while(*symbol == SEMICOLON) {
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
                proposition (file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
            }
            if (*symbol == END) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            }
            else {
                showErrorCode(file, errorFile, errorFileName, 12, str);
            }
            break;
        case IF:
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            condition(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
            jumpPosition = *memoryLimit;
            if(*symbol == THEN) {
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            }   
            else {
                showErrorCode(file, errorFile, errorFileName, 13, str);
            } 
            proposition (file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
            jumpDistance = *memoryLimit - jumpPosition;
            generateIntIn(jumpDistance, jumpPosition - 4, memory);
            break;
        case WHILE:
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            cyclePosition = *memoryLimit;
            condition (file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
            jumpPosition = *memoryLimit;
            if(*symbol == DO) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            }   
            else {
                showErrorCode(file, errorFile, errorFileName, 14, str);
            } 
            proposition(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
            cycleDistance = cyclePosition - (*memoryLimit + 5) ;
            generateByte(0xE9, memory, memoryLimit);
            generateInt(cycleDistance, memory, memoryLimit);
            jumpDistance = *memoryLimit - jumpPosition;
            generateIntIn(jumpDistance, jumpPosition - 4, memory);

            break;
        case READLN:
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            if (*symbol == OPENPARENTHESES) {
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 15, str);
            }
            if(*symbol == IDENT) {
                positionIdent = search(table, 0, base + displacement, str);
                if(positionIdent == -1) {
                    showErrorCode(file, errorFile, errorFileName, 18, str);
                } else if (table[positionIdent].type != VAR) {
                    showErrorCode(file, errorFile, errorFileName, 21, str);
                }

                distanceToRoutine = (0x590) - (*memoryLimit + 5); // 1424.
                generateByte(0xE8, memory, memoryLimit); // CALL.
                generateInt(distanceToRoutine, memory, memoryLimit);
                generateByte(0x89, memory, memoryLimit); // MOV [EDI+abcdefgh], EAX.
                generateByte(0x87, memory, memoryLimit);
                generateInt(table[positionIdent].value, memory, memoryLimit); // // abcdefgh.
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 3, str);
            } 
            while(*symbol == COMMA ) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                if (*symbol == IDENT){
                    positionIdent = search(table, 0, base + displacement, str);
                    if(positionIdent == -1) {
                    showErrorCode(file, errorFile, errorFileName, 18, str);
                } else if (table[positionIdent].type != VAR) {
                    showErrorCode(file, errorFile, errorFileName, 21, str);
                }   

                distanceToRoutine = (0x590) - (*memoryLimit + 5); // 1424.
                generateByte(0xE8, memory, memoryLimit); // CALL.
                generateInt(distanceToRoutine, memory, memoryLimit);
                generateByte(0x89, memory, memoryLimit); // MOV [EDI+abcdefgh], EAX.
                generateByte(0x87, memory, memoryLimit);
                generateInt(table[positionIdent].value, memory, memoryLimit);
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                } else {
                    showErrorCode(file, errorFile, errorFileName, 3, str);
                }
            }
            if (*symbol == CLOSEPARENTHESES){
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 16, str);
            }
            break;    
        case WRITELN:
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            if (*symbol == OPENPARENTHESES) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                if (*symbol == STRING) {                    
                    startStr = 0x401000 + (*memoryLimit - 0x200) + 15;
                    generateByte(0xB8, memory, memoryLimit); // MOV EAX, abcdefgh
                    generateInt(startStr, memory, memoryLimit);
                    distanceToRoutine = 0x3E0 - (*memoryLimit + 5) ; // 992, muestra por consola cadena
                    generateByte(0xE8, memory, memoryLimit); // CALL
                    generateInt(distanceToRoutine, memory, memoryLimit);
                    generateByte(0xE9, memory, memoryLimit); // JUMP
                    generateInt(strlen(str) -1, memory, memoryLimit);
                    for(int i = 1; i < strlen(str) -1; i++){
                        generateByte(str[i], memory, memoryLimit); // caracteres de la cadena
                    }
                    
                    generateByte(0x00, memory, memoryLimit); // 0
                    getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                } else {
                    expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
                    generateByte(0x58, memory, memoryLimit); // POP EAX
                    distanceToRoutine = 0x420 - (*memoryLimit + 5) ; // 1056, muestra por consola nro en EAX
                    generateByte(0xE8, memory, memoryLimit); // CALL
                    generateInt(distanceToRoutine, memory, memoryLimit);
                }
                while(*symbol == COMMA) {
                    getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                    if (*symbol == STRING) {
                        startStr = 0x401000 + (*memoryLimit - 0x200) + 15;
                        generateByte(0xB8, memory, memoryLimit); // MOV EAX, abcdefgh
                        generateInt(startStr, memory, memoryLimit);
                        distanceToRoutine = 0x3E0 - (*memoryLimit + 5) ; // 992, muestra por consola cadena
                        generateByte(0xE8, memory, memoryLimit); // CALL
                        generateInt(distanceToRoutine, memory, memoryLimit);
                        generateByte(0xE9, memory, memoryLimit); // JUMP
                        generateInt(strlen(str) - 1, memory, memoryLimit);
                        for(int i = 1; i < strlen(str) -1; i++){
                            generateByte(str[i], memory, memoryLimit); // caracteres de la cadena
                        }
                        generateByte(0x00, memory, memoryLimit); // 0
                        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                    } else {
                        expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
                        generateByte(0x58, memory, memoryLimit); // POP EAX
                        distanceToRoutine = 0x420 - (*memoryLimit + 5) ; // 1056, muestra por consola nro en EAX
                        generateByte(0xE8, memory, memoryLimit); // CALL
                        generateInt(distanceToRoutine, memory, memoryLimit);
                    }
                }
                if (*symbol == CLOSEPARENTHESES){
                    getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                } else {
                    showErrorCode(file, errorFile, errorFileName, 16, str);
                }
            }
            distanceToRoutine = 0x410 - (*memoryLimit + 5) ; // 1040, genera salto de linea
            generateByte(0xE8, memory, memoryLimit); // CALL
            generateInt(distanceToRoutine, memory, memoryLimit);
            break;   
        case WRITE:
            getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            if(*symbol == OPENPARENTHESES) {
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 15, str);
            }
            if(*symbol == STRING) {    
                startStr = 0x401000 + (*memoryLimit - 0x200) + 15;
                generateByte(0xB8, memory, memoryLimit); // MOV EAX, abcdefgh
                generateInt(startStr, memory, memoryLimit);
                distanceToRoutine = 0x3E0 - (*memoryLimit + 5) ; // 992, muestra por consola cadena
                generateByte(0xE8, memory, memoryLimit); // CALL
                generateInt(distanceToRoutine, memory, memoryLimit);
                generateByte(0xE9, memory, memoryLimit); // JUMP
                generateInt(strlen(str) -1, memory, memoryLimit);
                for(int i = 1; i < strlen(str) -1; i++){
                    generateByte(str[i], memory, memoryLimit); // caracteres de la cadena
                }

                generateByte(0x00, memory, memoryLimit); // 0
                getSymbol(file, errorFile, str, remaining, symbol,  lineCount);
            } else {
                expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);   
                generateByte(0x58, memory, memoryLimit); // POP EAX
                distanceToRoutine = 0x420 - (*memoryLimit + 5) ; // 1056, muestra por consola nro en EAX
                generateByte(0xE8, memory, memoryLimit); // CALL
                generateInt(distanceToRoutine, memory, memoryLimit);
            }
            while(*symbol == COMMA) {
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                if (*symbol == STRING) {
                    startStr = 0x401000 + (*memoryLimit - 0x200) + 15;
                    generateByte(0xB8, memory, memoryLimit); // MOV EAX, abcdefgh
                    generateInt(startStr, memory, memoryLimit);
                    distanceToRoutine = 0x3E0 - (*memoryLimit + 5) ; // 992, muestra por consola cadena
                    generateByte(0xE8, memory, memoryLimit); // CALL
                    generateInt(distanceToRoutine, memory, memoryLimit);
                    generateByte(0xE9, memory, memoryLimit); // JUMP
                    generateInt(strlen(str) - 1, memory, memoryLimit);
                    for(int i = 1; i < strlen(str) -1; i++){
                        generateByte(str[i], memory, memoryLimit); // caracteres de la cadena
                    }

                    generateByte(0x00, memory, memoryLimit); // 0
                    getSymbol(file, errorFile, str, remaining, symbol, lineCount);
                } else {
                    expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
                    generateByte(0x58, memory, memoryLimit); // POP EAX
                    distanceToRoutine = 0x420 - (*memoryLimit + 5) ; // 1056, muestra por consola nro en EAX
                    generateByte(0xE8, memory, memoryLimit); // CALL
                    generateInt(distanceToRoutine, memory, memoryLimit);
                }
            }
            if (*symbol == CLOSEPARENTHESES){
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            } else {
                showErrorCode(file, errorFile, errorFileName, 16, str);
            }
            break;
    }
}

void condition(FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit,
                string str, string remaining, symbol_t* symbol, int* lineCount,
                int base, int displacement, identifierTable table) {
    
    if (*symbol == ODD) {
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
        // Los bytes generados luego de la expresión que sucede a ODD son:
        generateByte(0x58, memory, memoryLimit); // POP EAX.
        generateByte(0xA8, memory, memoryLimit); // TEST(doesn´t modify to AL) AL, ab / AND(modify to AL) AL, 01.
        generateByte(0x01, memory, memoryLimit);
        generateByte(0x7B, memory, memoryLimit);// JPO 05.
        generateByte(0x05, memory, memoryLimit);        
        generateByte(0xE9, memory, memoryLimit); // JMP dir.
        generateInt(0, memory, memoryLimit);        
    } else {
        expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
        symbol_t operationSymbol = *symbol;
        switch(*symbol){
            case EQUAL:
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            break;
            case DISTINCT:
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            break;
            case GREATER:
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            break;
            case GREATEROREQUAL:
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            break;
            case LESS:
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            break;
            case LESSOREQUAL:
                getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            break;
            default :
                showErrorCode(file, errorFile, errorFileName, 8, str);
        }

        expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol,  lineCount, base, displacement, table);
        /*
            Las instrucciones generadas luego de la segunda expresión en la parte
            inferior del grafo son todas iguales, salvo por un salto condicional (de
            2 bytes) que es específico del operador booleano:
        */
        generateByte(0x58, memory, memoryLimit); // POP EAX.
        generateByte(0x5B, memory, memoryLimit); // POP EBX.
        generateByte(0x39, memory, memoryLimit); // CMP EBX, EAX
        generateByte(0xC3, memory, memoryLimit);
        
        switch(operationSymbol) {
            case EQUAL:
                generateByte(0x74, memory, memoryLimit); //JE.
            break;
            case DISTINCT:
                generateByte(0x75, memory, memoryLimit); // JNE.
            break;
            case GREATER:
                generateByte(0x7F, memory, memoryLimit); // JG.
            break;
            case GREATEROREQUAL:
                generateByte(0x7D, memory, memoryLimit); // JGE.
            break;
            case LESS:
                generateByte(0x7C, memory, memoryLimit); // JL.
            break;
            case LESSOREQUAL:
                generateByte(0x7E, memory, memoryLimit); // JLE.
            break;
        }
        generateByte(0x05, memory, memoryLimit);
       /*
            Como no se conoce de antemano la cantidad de bytes que deben
            saltearse, se genera un salto E9 00 00 00 00 "para reservar el lugar".
            Luego de generar las instrucciones de la proposición, se debe volver    
            atrás para corregir el destino del salto. Esto se conoce como "fix-up".
       */
        generateByte(0xE9, memory, memoryLimit); // JMP dir.
        generateInt(0, memory, memoryLimit);
    }
}

void expression(FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit,
                string str, string remaining, symbol_t* symbol, int* lineCount, 
                int base, int displacement, identifierTable table) {
    
    int positionIdent = 0;
    symbol_t signOne = *symbol;

    if (*symbol == PLUS) {
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
    } else if (*symbol == MINUS) {
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
    }
    
    term(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
    if(signOne == MINUS) {
        generateByte(0x58, memory, memoryLimit); // POP EAX
        generateByte(0xF7, memory, memoryLimit); // NEG EAX
        generateByte(0xD8, memory, memoryLimit); 
        generateByte(0x50, memory, memoryLimit); // POP EBX
    }
    while (*symbol == PLUS || *symbol == MINUS) {
        symbol_t signTwo = *symbol;
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        term(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
        if (signTwo == PLUS) {
            generateByte(0x58, memory, memoryLimit); // POP EAX
            generateByte(0x5B, memory, memoryLimit); // POP EBX
            generateByte(0x01, memory, memoryLimit); // ADD EAX, EBX
            generateByte(0xD8, memory, memoryLimit); 
            generateByte(0x50, memory, memoryLimit); // PUSH EAX
        } else { // MINUS
            generateByte(0x58, memory, memoryLimit); // POP EAX
            generateByte(0x5B, memory, memoryLimit); // POP EBX
            generateByte(0x93, memory, memoryLimit); // XCHG EAX, EBX
            generateByte(0x29, memory, memoryLimit); // SUB EAX, EBX
            generateByte(0xD8, memory, memoryLimit);
            generateByte(0x50, memory, memoryLimit); // PUSH EAX
        }
    }
}

void term(FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit,
         string str, string remaining, symbol_t* symbol, int* lineCount,
         int base, int displacement, identifierTable table) {

    symbol_t sign;
     
    factor(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
    while (*symbol == MULTIPLY || *symbol == DIVIDE ) {
        sign =  *symbol;
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        factor(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);      
        if (sign == MULTIPLY) {
            generateByte(0x58, memory, memoryLimit); // POP EAX.
            generateByte(0x5B, memory, memoryLimit); // POP EBX.
            generateByte(0xF7, memory, memoryLimit); // ISMUL EBX.
            generateByte(0xEB, memory, memoryLimit); 
            generateByte(0x50, memory, memoryLimit); // PUSH EAX.
        } else { // DIVIDE
            generateByte(0x58, memory, memoryLimit); // POP EAX.
            generateByte(0x5B, memory, memoryLimit); // POP EBX.
            generateByte(0x93, memory, memoryLimit); // XCHG EAX, EBX.
            generateByte(0x99, memory, memoryLimit); // CDQ 1001 (9) -> 0000 .. 1001 / 1111 1001 (-9) -> 1111...11111001.
            generateByte(0xF7, memory, memoryLimit); // IDIV EBX
            generateByte(0xFB, memory, memoryLimit);
            generateByte(0x50, memory, memoryLimit); // PUSH EAX
        }
    }
}

void factor (FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit,
             string str, string remaining, symbol_t* symbol, int* lineCount,
             int base, int displacement, identifierTable table) {

    int positionIdent = 0;
    
    switch (*symbol) {
    case IDENT:
        positionIdent = search(table, 0, base + displacement, str);
        if (positionIdent == -1){
            showErrorCode(file, errorFile, errorFileName, 18, str);
        } else if (table[positionIdent].type == PROCEDURE){
            showErrorCode(file, errorFile, errorFileName, 19, str);
        } else if (table[positionIdent].type == VAR) {
            generateByte(0x8B, memory, memoryLimit); // MOV EAX, [EDI+abcdefgh]
            generateByte(0x87, memory, memoryLimit);
            generateInt(table[positionIdent].value, memory, memoryLimit);
            generateByte(0x50, memory, memoryLimit); // PUSH EAX
        } else { // CONST
            generateByte(0xB8, memory, memoryLimit); // MOV EAX, abcdefgh
            generateInt(table[positionIdent].value, memory, memoryLimit);
            generateByte(0x50, memory, memoryLimit); // PUSH EAX
        }
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        break;
    case NUMBER:
        generateByte(0xB8, memory, memoryLimit); // MOV EAX, abcdefgh
        generateInt(atoi(str), memory, memoryLimit);
        generateByte(0x50, memory, memoryLimit); // PUSH EAX
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        break;
    case OPENPARENTHESES:
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
        if (*symbol == CLOSEPARENTHESES) {
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 10, str);
        }
        break;
    case SQR:
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        if (*symbol == OPENPARENTHESES) {
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
            expression(file, errorFile, errorFileName, memory, memoryLimit, str, remaining, symbol, lineCount, base, displacement, table);
        } else {
            showErrorCode(file, errorFile, errorFileName, 15, str);
        }
        if (*symbol == CLOSEPARENTHESES) {
            getSymbol(file, errorFile, str, remaining, symbol, lineCount);
        } else {
            showErrorCode(file, errorFile, errorFileName, 10, str);
        }

        generateByte(0x58, memory, memoryLimit); // POP EAX
        memory[(*memoryLimit)--];
        generateByte(0x5B, memory, memoryLimit); // POP EBX
        generateByte(0xF7, memory, memoryLimit); // ISMUL EBX.
        generateByte(0xEB, memory, memoryLimit);
        generateByte(0x50, memory, memoryLimit); // PUSH EAX.
        break;
    default:
        showErrorCode(file, errorFile,errorFileName, 9, str);
        break;
    }
}

void program(FILE* file, FILE* errorFile, string errorFileName, vectorBytes_t memory, int* memoryLimit, 
             string str, string remaining, symbol_t* symbol, int* lineCount, identifierTable table) {
    
    memory[0] = 0x4D; // 'M'  (Magic number)
    memory[1] = 0x5A; // 'Z'
    memory[2] = 0x60; // Bytes on last block
    memory[3] = 0x01; // (1 bl. = 512 bytes)
    memory[4] = 0x01; // Number of blocks
    memory[5] = 0x00; // in the EXE file
    memory[6] = 0x00; // Number of re-
    memory[7] = 0x00; // location entries
    memory[8] = 0x04; // Size of header
    memory[9] = 0x00; // (x 16 bytes)
    memory[10] = 0x00; // Minimum extra
    memory[11] = 0x00; // paragraphs needed
    memory[12] = 0xFF; // Maximum extra
    memory[13] = 0xFF; // paragraphs needed
    memory[14] = 0x00; // Initial (relative)
    memory[15] = 0x00; // SS value
    memory[16] = 0x60; // Initial SP value
    memory[17] = 0x01;
    memory[18] = 0x00; // Checksum
    memory[19] = 0x00;
    memory[20] = 0x00; // Initial IP value
    memory[21] = 0x00;
    memory[22] = 0x00; // Initial (relative)
    memory[23] = 0x00; // CS value
    memory[24] = 0x40; // Offset of the 1st
    memory[25] = 0x00; // relocation item
    memory[26] = 0x00; // Overlay number.
    memory[27] = 0x00; // (0 = main program)
    memory[28] = 0x00; // Reserved word
    memory[29] = 0x00;
    memory[30] = 0x00; // Reserved word
    memory[31] = 0x00;
    memory[32] = 0x00; // Reserved word
    memory[33] = 0x00;
    memory[34] = 0x00; // Reserved word
    memory[35] = 0x00;
    memory[36] = 0x00; // OEM identifier
    memory[37] = 0x00;
    memory[38] = 0x00; // OEM information
    memory[39] = 0x00;
    memory[40] = 0x00; // Reserved word
    memory[41] = 0x00;
    memory[42] = 0x00; // Reserved word
    memory[43] = 0x00;
    memory[44] = 0x00; // Reserved word
    memory[45] = 0x00;
    memory[46] = 0x00; // Reserved word
    memory[47] = 0x00;
    memory[48] = 0x00; // Reserved word
    memory[49] = 0x00;
    memory[50] = 0x00; // Reserved word
    memory[51] = 0x00;
    memory[52] = 0x00; // Reserved word
    memory[53] = 0x00;
    memory[54] = 0x00; // Reserved word
    memory[55] = 0x00;
    memory[56] = 0x00; // Reserved word
    memory[57] = 0x00;
    memory[58] = 0x00; // Reserved word
    memory[59] = 0x00;
    memory[60] = 0xA0; // Start of the COFF
    memory[61] = 0x00; // header
    memory[62] = 0x00;
    memory[63] = 0x00;
    memory[64] = 0x0E; // PUSH CS
    memory[65] = 0x1F; // POP DS
    memory[66] = 0xBA; // MOV DX,000E
    memory[67] = 0x0E;
    memory[68] = 0x00;
    memory[69] = 0xB4; // MOV AH,09
    memory[70] = 0x09;
    memory[71] = 0xCD; // INT 21
    memory[72] = 0x21;
    memory[73] = 0xB8; // MOV AX,4C01
    memory[74] = 0x01;
    memory[75] = 0x4C;
    memory[76] = 0xCD; // INT 21
    memory[77] = 0x21;
    memory[78] = 0x54; // 'T'
    memory[79] = 0x68; // 'h'
    memory[80] = 0x69; // 'i'
    memory[81] = 0x73; // 's'
    memory[82] = 0x20; // ' '
    memory[83] = 0x70; // 'p'
    memory[84] = 0x72; // 'r'
    memory[85] = 0x6F; // 'o'
    memory[86] = 0x67; // 'g'
    memory[87] = 0x72; // 'r'
    memory[88] = 0x61; // 'a'
    memory[89] = 0x6D; // 'm'
    memory[90] = 0x20; // ' '
    memory[91] = 0x69; // 'i'
    memory[92] = 0x73; // 's'
    memory[93] = 0x20; // ' '
    memory[94] = 0x61; // 'a'
    memory[95] = 0x20; // ' '
    memory[96] = 0x57; // 'W'
    memory[97] = 0x69; // 'i'
    memory[98] = 0x6E; // 'n'
    memory[99] = 0x33; // '3'
    memory[100] = 0x32; // '2'
    memory[101] = 0x20; // ' '
    memory[102] = 0x63; // 'c'
    memory[103] = 0x6F; // 'o'
    memory[104] = 0x6E; // 'n'
    memory[105] = 0x73; // 's'
    memory[106] = 0x6F; // 'o'
    memory[107] = 0x6C; // 'l'
    memory[108] = 0x65; // 'e'
    memory[109] = 0x20; // ' '
    memory[110] = 0x61; // 'a'
    memory[111] = 0x70; // 'p'
    memory[112] = 0x70; // 'p'
    memory[113] = 0x6C; // 'l'
    memory[114] = 0x69; // 'i'
    memory[115] = 0x63; // 'c'
    memory[116] = 0x61; // 'a'
    memory[117] = 0x74; // 't'
    memory[118] = 0x69; // 'i'
    memory[119] = 0x6F; // 'o'
    memory[120] = 0x6E; // 'n'
    memory[121] = 0x2E; // '.'
    memory[122] = 0x20; // ' '
    memory[123] = 0x49; // 'I'
    memory[124] = 0x74; // 't'
    memory[125] = 0x20; // ' '
    memory[126] = 0x63; // 'c'
    memory[127] = 0x61; // 'a'
    memory[128] = 0x6E; // 'n'
    memory[129] = 0x6E; // 'n'
    memory[130] = 0x6F; // 'o'
    memory[131] = 0x74; // 't'
    memory[132] = 0x20; // ' '
    memory[133] = 0x62; // 'b'
    memory[134] = 0x65; // 'e'
    memory[135] = 0x20; // ' '
    memory[136] = 0x72; // 'r'
    memory[137] = 0x75; // 'u'
    memory[138] = 0x6E; // 'n'
    memory[139] = 0x20; // ' '
    memory[140] = 0x75; // 'u'
    memory[141] = 0x6E; // 'n'
    memory[142] = 0x64; // 'd'
    memory[143] = 0x65; // 'e'
    memory[144] = 0x72; // 'r'
    memory[145] = 0x20; // ' '
    memory[146] = 0x4D; // 'M'
    memory[147] = 0x53; // 'S'
    memory[148] = 0x2D; // '-'
    memory[149] = 0x44; // 'D'
    memory[150] = 0x4F; // 'O'
    memory[151] = 0x53; // 'S'
    memory[152] = 0x2E; // '.'
    memory[153] = 0x0D; // Carriage return
    memory[154] = 0x0A; // Line feed
    memory[155] = 0x24; // String end ('$')
    memory[156] = 0x00;
    memory[157] = 0x00;
    memory[158] = 0x00;
    memory[159] = 0x00;

    /* COFF HEADER - 8 Standard fields */

    memory[160] = 0x50; // 'P'
    memory[161] = 0x45; // 'E'
    memory[162] = 0x00; // '\0'
    memory[163] = 0x00; // '\0'
    memory[164] = 0x4C; // Machine:
    memory[165] = 0x01; // >= Intel 386
    memory[166] = 0x01; // Number of
    memory[167] = 0x00; // sections
    memory[168] = 0x00; // Time/Date stamp
    memory[169] = 0x00;
    memory[170] = 0x53;
    memory[171] = 0x4C;
    memory[172] = 0x00; // Pointer to symbol
    memory[173] = 0x00; // table
    memory[174] = 0x00; // (deprecated)
    memory[175] = 0x00;
    memory[176] = 0x00; // Number of symbols
    memory[177] = 0x00; // (deprecated)
    memory[178] = 0x00;
    memory[179] = 0x00;
    memory[180] = 0xE0; // Size of optional
    memory[181] = 0x00; // header
    memory[182] = 0x02; // Characteristics:
    memory[183] = 0x01; // 32BIT_MACHINE EXE

    /* OPTIONAL HEADER - 8 Standard fields */
    /*  (For image files, it is required)  */

    memory[184] = 0x0B; // Magic number
    memory[185] = 0x01; // (010B = PE32)
    memory[186] = 0x01;// Maj.Linker Version
    memory[187] = 0x00;// Min.Linker Version
    memory[188] = 0x00; // Size of code
    memory[189] = 0x06; // (text) section
    memory[190] = 0x00;
    memory[191] = 0x00;
    memory[192] = 0x00; // Size of
    memory[193] = 0x00; // initialized data
    memory[194] = 0x00; // section
    memory[195] = 0x00;
    memory[196] = 0x00; // Size of
    memory[197] = 0x00; // uninitialized
    memory[198] = 0x00; // data section
    memory[199] = 0x00;
    memory[200] = 0x00; // Starting address
    memory[201] = 0x15; // relative to the
    memory[202] = 0x00; // image base
    memory[203] = 0x00;
    memory[204] = 0x00; // Base of code
    memory[205] = 0x10;
    memory[206] = 0x00;
    memory[207] = 0x00;

    /* OPT.HEADER - 1 PE32 specific field */

    memory[208] = 0x00; // Base of data
    memory[209] = 0x20;
    memory[210] = 0x00;
    memory[211] = 0x00;

    /* OPT.HEADER - 21 Win-Specific Fields */

    memory[212] = 0x00; // Image base
    memory[213] = 0x00; // (Preferred
    memory[214] = 0x40; // address of image
    memory[215] = 0x00; // when loaded)
    memory[216] = 0x00; // Section alignment
    memory[217] = 0x10;
    memory[218] = 0x00;
    memory[219] = 0x00;
    memory[220] = 0x00; // File alignment
    memory[221] = 0x02; // (Default is 512)
    memory[222] = 0x00;
    memory[223] = 0x00;
    memory[224] = 0x04; // Major OS version
    memory[225] = 0x00;
    memory[226] = 0x00; // Minor OS version
    memory[227] = 0x00;
    memory[228] = 0x00;// Maj. image version
    memory[229] = 0x00;
    memory[230] = 0x00;// Min. image version
    memory[231] = 0x00;
    memory[232] = 0x04;// Maj.subsystem ver.
    memory[233] = 0x00;
    memory[234] = 0x00;// Min.subsystem ver.
    memory[235] = 0x00;
    memory[236] = 0x00; // Win32 version
    memory[237] = 0x00; // (Reserved, must
    memory[238] = 0x00; // be zero)
    memory[239] = 0x00;
    memory[240] = 0x00;// Size of image
    memory[241] = 0x20;// (It must be a
    memory[242] = 0x00;// multiple of the
    memory[243] = 0x00;// section alignment)
    memory[244] = 0x00; // Size of headers
    memory[245] = 0x02; // (rounded up to a
    memory[246] = 0x00; // multiple of the
    memory[247] = 0x00; // file alignment)
    memory[248] = 0x00; // Checksum
    memory[249] = 0x00;
    memory[250] = 0x00;
    memory[251] = 0x00;
    memory[252] = 0x03; // Windows subsyste
    memory[253] = 0x00; // (03 = console)
    memory[254] = 0x00; // DLL characteristics
    memory[255] = 0x00; //
    memory[256] = 0x00; // Size of stack
    memory[257] = 0x00; // reserve
    memory[258] = 0x10;
    memory[259] = 0x00;
    memory[260] = 0x00; // Size of stack
    memory[261] = 0x10; // commit
    memory[262] = 0x00;
    memory[263] = 0x00;
    memory[264] = 0x00; // Size of heap
    memory[265] = 0x00; // reserve
    memory[266] = 0x10;
    memory[267] = 0x00;
    memory[268] = 0x00; // Size of heap
    memory[269] = 0x10; // commit
    memory[270] = 0x00;
    memory[271] = 0x00;
    memory[272] = 0x00; // Loader flags
    memory[273] = 0x00; // (Reserved, must
    memory[274] = 0x00; // be zero)
    memory[275] = 0x00;
    memory[276] = 0x10; // Number of
    memory[277] = 0x00; // relative virtual
    memory[278] = 0x00; // addresses (RVAs)
    memory[279] = 0x00;

    /* OPT. HEADER - 16 Data Directories */
    memory[280] = 0x00; // Export Table
    memory[281] = 0x00;
    memory[282] = 0x00;
    memory[283] = 0x00;
    memory[284] = 0x00;
    memory[285] = 0x00;
    memory[286] = 0x00;
    memory[287] = 0x00;
    memory[288] = 0x1C; // Import Table
    memory[289] = 0x10;
    memory[290] = 0x00;
    memory[291] = 0x00;
    memory[292] = 0x28;
    memory[293] = 0x00;
    memory[294] = 0x00;
    memory[295] = 0x00;
    memory[296] = 0x00; // Resource Table
    memory[297] = 0x00;
    memory[298] = 0x00;
    memory[299] = 0x00;
    memory[300] = 0x00;
    memory[301] = 0x00;
    memory[302] = 0x00;
    memory[303] = 0x00;
    memory[304] = 0x00; // Exception Table
    memory[305] = 0x00;
    memory[306] = 0x00;
    memory[307] = 0x00;
    memory[308] = 0x00;
    memory[309] = 0x00;
    memory[310] = 0x00;
    memory[311] = 0x00;
    memory[312] = 0x00; // Certificate Table
    memory[313] = 0x00;
    memory[314] = 0x00;
    memory[315] = 0x00;
    memory[316] = 0x00;
    memory[317] = 0x00;
    memory[318] = 0x00;
    memory[319] = 0x00;
    memory[320] = 0x00; // Base Relocation
    memory[321] = 0x00; // Table
    memory[322] = 0x00;
    memory[323] = 0x00;
    memory[324] = 0x00;
    memory[325] = 0x00;
    memory[326] = 0x00;
    memory[327] = 0x00;
    memory[328] = 0x00; // Debug
    memory[329] = 0x00;
    memory[330] = 0x00;
    memory[331] = 0x00;
    memory[332] = 0x00;
    memory[333] = 0x00;
    memory[334] = 0x00;
    memory[335] = 0x00;
    memory[336] = 0x00; // Architecture
    memory[337] = 0x00;
    memory[338] = 0x00;
    memory[339] = 0x00;
    memory[340] = 0x00;
    memory[341] = 0x00;
    memory[342] = 0x00;
    memory[343] = 0x00;
    memory[344] = 0x00; // Global Ptr
    memory[345] = 0x00;
    memory[346] = 0x00;
    memory[347] = 0x00;
    memory[348] = 0x00;
    memory[349] = 0x00;
    memory[350] = 0x00;
    memory[351] = 0x00;
    memory[352] = 0x00; // TLS Table
    memory[353] = 0x00;
    memory[354] = 0x00;
    memory[355] = 0x00;
    memory[356] = 0x00;
    memory[357] = 0x00;
    memory[358] = 0x00;
    memory[359] = 0x00;
    memory[360] = 0x00; // Load Conf
    memory[361] = 0x00;
    memory[362] = 0x00;
    memory[363] = 0x00;
    memory[364] = 0x00;
    memory[365] = 0x00;
    memory[366] = 0x00;
    memory[367] = 0x00;
    memory[368] = 0x00; // Bound Imp
    memory[369] = 0x00;
    memory[370] = 0x00;
    memory[371] = 0x00;
    memory[372] = 0x00;
    memory[373] = 0x00;
    memory[374] = 0x00;
    memory[375] = 0x00;
    memory[376] = 0x00; // IAT
    memory[377] = 0x10;
    memory[378] = 0x00;
    memory[379] = 0x00;
    memory[380] = 0x1C;
    memory[381] = 0x00;
    memory[382] = 0x00;
    memory[383] = 0x00;
    memory[384] = 0x00; // Delay Imp
    memory[385] = 0x00; // Descripto
    memory[386] = 0x00;
    memory[387] = 0x00;
    memory[388] = 0x00;
    memory[389] = 0x00;
    memory[390] = 0x00;
    memory[391] = 0x00;
    memory[392] = 0x00; // CLR Runti
    memory[393] = 0x00; // Header
    memory[394] = 0x00;
    memory[395] = 0x00;
    memory[396] = 0x00;
    memory[397] = 0x00;
    memory[398] = 0x00;
    memory[399] = 0x00;
    memory[400] = 0x00; // Reserved,
    memory[401] = 0x00; // zero
    memory[402] = 0x00;
    memory[403] = 0x00;
    memory[404] = 0x00;
    memory[405] = 0x00;
    memory[406] = 0x00;
    memory[407] = 0x00;

    /* SECTIONS TABLE (40 bytes per e
    /* FIRST ENTRY: TEXT HEADER */
    memory[408] = 0x2E; // '.'
    memory[409] = 0x74; // 't'
    memory[410] = 0x65; // 'e'
    memory[411] = 0x78; // 'x'
    memory[412] = 0x74; // 't'
    memory[413] = 0x00;
    memory[414] = 0x00;
    memory[415] = 0x00;
    memory[416] = 0x24; // Virtual size
    memory[417] = 0x05;
    memory[418] = 0x00;
    memory[419] = 0x00;
    memory[420] = 0x00; // Virtual address
    memory[421] = 0x10;
    memory[422] = 0x00;
    memory[423] = 0x00;
    memory[424] = 0x00; // Size of raw data
    memory[425] = 0x06;
    memory[426] = 0x00;
    memory[427] = 0x00;
    memory[428] = 0x00; // Pointer to
    memory[429] = 0x02; // raw data
    memory[430] = 0x00;
    memory[431] = 0x00;
    memory[432] = 0x00; // Pointer to
    memory[433] = 0x00; // relocations
    memory[434] = 0x00;
    memory[435] = 0x00;
    memory[436] = 0x00; // Pointer to
    memory[437] = 0x00; // line numbers
    memory[438] = 0x00;
    memory[439] = 0x00;
    memory[440] = 0x00; // Number of
    memory[441] = 0x00; // relocations
    memory[442] = 0x00; // Number of
    memory[443] = 0x00; // line numbers
    memory[444] = 0x20; // Characteristics
    memory[445] = 0x00; // (Readable,
    memory[446] = 0x00; // Writeable &
    memory[447] = 0xE0; // Executable)
    memory[448]=0x00;
    memory[449]=0x00;
    memory[450]=0x00;
    memory[451]=0x00;
    memory[452]=0x00;
    memory[453]=0x00;
    memory[454]=0x00;
    memory[455]=0x00;
    memory[456]=0x00;
    memory[457]=0x00;
    memory[458]=0x00;
    memory[459]=0x00;
    memory[460]=0x00;
    memory[461]=0x00;
    memory[462]=0x00;
    memory[463]=0x00;
    memory[464]=0x00;
    memory[465]=0x00;
    memory[466]=0x00;
    memory[467]=0x00;
    memory[468]=0x00;
    memory[469]=0x00;
    memory[470]=0x00;
    memory[471]=0x00;
    memory[472]=0x00;
    memory[473]=0x00;
    memory[474]=0x00;
    memory[475]=0x00;
    memory[476]=0x00;
    memory[477]=0x00;
    memory[478]=0x00;
    memory[479]=0x00;
    memory[480]=0x00;
    memory[481]=0x00;
    memory[482]=0x00;
    memory[483]=0x00;
    memory[484]=0x00;
    memory[485]=0x00;
    memory[486]=0x00;
    memory[487]=0x00;
    memory[488]=0x00;
    memory[489]=0x00;
    memory[490]=0x00;
    memory[491]=0x00;
    memory[492]=0x00;
    memory[493]=0x00;
    memory[494]=0x00;
    memory[495]=0x00;
    memory[496]=0x00;
    memory[497]=0x00;
    memory[498]=0x00;
    memory[499]=0x00;
    memory[500]=0x00;
    memory[501]=0x00;
    memory[502]=0x00;
    memory[503]=0x00;
    memory[504]=0x00;
    memory[505]=0x00;
    memory[506]=0x00;
    memory[507]=0x00;
    memory[508]=0x00;
    memory[509]=0x00;
    memory[510]=0x00;
    memory[511]=0x00;
    memory[512]=0x6E;
    memory[513]=0x10;
    memory[514]=0x00;
    memory[515]=0x00;
    memory[516]=0x7C;
    memory[517]=0x10;
    memory[518]=0x00;
    memory[519]=0x00;
    memory[520]=0x8C;
    memory[521]=0x10;
    memory[522]=0x00;
    memory[523]=0x00;
    memory[524]=0x98;
    memory[525]=0x10;
    memory[526]=0x00;
    memory[527]=0x00;
    memory[528]=0xA4;
    memory[529]=0x10;
    memory[530]=0x00;
    memory[531]=0x00;
    memory[532]=0xB6;
    memory[533]=0x10;
    memory[534]=0x00;
    memory[535]=0x00;
    memory[536]=0x00;
    memory[537]=0x00;
    memory[538]=0x00;
    memory[539]=0x00;
    memory[540]=0x52;
    memory[541]=0x10;
    memory[542]=0x00;
    memory[543]=0x00;
    memory[544]=0x00;
    memory[545]=0x00;
    memory[546]=0x00;
    memory[547]=0x00;
    memory[548]=0x00;
    memory[549]=0x00;
    memory[550]=0x00;
    memory[551]=0x00;
    memory[552]=0x44;
    memory[553]=0x10;
    memory[554]=0x00;
    memory[555]=0x00;
    memory[556]=0x00;
    memory[557]=0x10;
    memory[558]=0x00;
    memory[559]=0x00;
    memory[560]=0x00;
    memory[561]=0x00;
    memory[562]=0x00;
    memory[563]=0x00;
    memory[564]=0x00;
    memory[565]=0x00;
    memory[566]=0x00;
    memory[567]=0x00;
    memory[568]=0x00;
    memory[569]=0x00;
    memory[570]=0x00;
    memory[571]=0x00;
    memory[572]=0x00;
    memory[573]=0x00;
    memory[574]=0x00;
    memory[575]=0x00;
    memory[576]=0x00;
    memory[577]=0x00;
    memory[578]=0x00;
    memory[579]=0x00;
    memory[580]=0x4B;
    memory[581]=0x45;
    memory[582]=0x52;
    memory[583]=0x4E;
    memory[584]=0x45;
    memory[585]=0x4C;
    memory[586]=0x33;
    memory[587]=0x32;
    memory[588]=0x2E;
    memory[589]=0x64;
    memory[590]=0x6C;
    memory[591]=0x6C;
    memory[592]=0x00;
    memory[593]=0x00;
    memory[594]=0x6E;
    memory[595]=0x10;
    memory[596]=0x00;
    memory[597]=0x00;
    memory[598]=0x7C;
    memory[599]=0x10;
    memory[600]=0x00;
    memory[601]=0x00;
    memory[602]=0x8C;
    memory[603]=0x10;
    memory[604]=0x00;
    memory[605]=0x00;
    memory[606]=0x98;
    memory[607]=0x10;
    memory[608]=0x00;
    memory[609]=0x00;
    memory[610]=0xA4;
    memory[611]=0x10;
    memory[612]=0x00;
    memory[613]=0x00;
    memory[614]=0xB6;
    memory[615]=0x10;
    memory[616]=0x00;
    memory[617]=0x00;
    memory[618]=0x00;
    memory[619]=0x00;
    memory[620]=0x00;
    memory[621]=0x00;
    memory[622]=0x00;
    memory[623]=0x00;
    memory[624]=0x45;
    memory[625]=0x78;
    memory[626]=0x69;
    memory[627]=0x74;
    memory[628]=0x50;
    memory[629]=0x72;
    memory[630]=0x6F;
    memory[631]=0x63;
    memory[632]=0x65;
    memory[633]=0x73;
    memory[634]=0x73;
    memory[635]=0x00;
    memory[636]=0x00;
    memory[637]=0x00;
    memory[638]=0x47;
    memory[639]=0x65;
    memory[640]=0x74;
    memory[641]=0x53;
    memory[642]=0x74;
    memory[643]=0x64;
    memory[644]=0x48;
    memory[645]=0x61;
    memory[646]=0x6E;
    memory[647]=0x64;
    memory[648]=0x6C;
    memory[649]=0x65;
    memory[650]=0x00;
    memory[651]=0x00;
    memory[652]=0x00;
    memory[653]=0x00;
    memory[654]=0x52;
    memory[655]=0x65;
    memory[656]=0x61;
    memory[657]=0x64;
    memory[658]=0x46;
    memory[659]=0x69;
    memory[660]=0x6C;
    memory[661]=0x65;
    memory[662]=0x00;
    memory[663]=0x00;
    memory[664]=0x00;
    memory[665]=0x00;
    memory[666]=0x57;
    memory[667]=0x72;
    memory[668]=0x69;
    memory[669]=0x74;
    memory[670]=0x65;
    memory[671]=0x46;
    memory[672]=0x69;
    memory[673]=0x6C;
    memory[674]=0x65;
    memory[675]=0x00;
    memory[676]=0x00;
    memory[677]=0x00;
    memory[678]=0x47;
    memory[679]=0x65;
    memory[680]=0x74;
    memory[681]=0x43;
    memory[682]=0x6F;
    memory[683]=0x6E;
    memory[684]=0x73;
    memory[685]=0x6F;
    memory[686]=0x6C;
    memory[687]=0x65;
    memory[688]=0x4D;
    memory[689]=0x6F;
    memory[690]=0x64;
    memory[691]=0x65;
    memory[692]=0x00;
    memory[693]=0x00;
    memory[694]=0x00;
    memory[695]=0x00;
    memory[696]=0x53;
    memory[697]=0x65;
    memory[698]=0x74;
    memory[699]=0x43;
    memory[700]=0x6F;
    memory[701]=0x6E;
    memory[702]=0x73;
    memory[703]=0x6F;
    memory[704]=0x6C;
    memory[705]=0x65;
    memory[706]=0x4D;
    memory[707]=0x6F;
    memory[708]=0x64;
    memory[709]=0x65;
    memory[710]=0x00;
    memory[711]=0x00;
    memory[712]=0x00;
    memory[713]=0x00;
    memory[714]=0x00;
    memory[715]=0x00;
    memory[716]=0x00;
    memory[717]=0x00;
    memory[718]=0x00;
    memory[719]=0x00;
    memory[720]=0x50;
    memory[721]=0xA2;
    memory[722]=0x1C;
    memory[723]=0x11;
    memory[724]=0x40;
    memory[725]=0x00;
    memory[726]=0x31;
    memory[727]=0xC0;
    memory[728]=0x03;
    memory[729]=0x05;
    memory[730]=0x2C;
    memory[731]=0x11;
    memory[732]=0x40;
    memory[733]=0x00;
    memory[734]=0x75;
    memory[735]=0x0D;
    memory[736]=0x6A;
    memory[737]=0xF5;
    memory[738]=0xFF;
    memory[739]=0x15;
    memory[740]=0x04;
    memory[741]=0x10;
    memory[742]=0x40;
    memory[743]=0x00;
    memory[744]=0xA3;
    memory[745]=0x2C;
    memory[746]=0x11;
    memory[747]=0x40;
    memory[748]=0x00;
    memory[749]=0x6A;
    memory[750]=0x00;
    memory[751]=0x68;
    memory[752]=0x30;
    memory[753]=0x11;
    memory[754]=0x40;
    memory[755]=0x00;
    memory[756]=0x6A;
    memory[757]=0x01;
    memory[758]=0x68;
    memory[759]=0x1C;
    memory[760]=0x11;
    memory[761]=0x40;
    memory[762]=0x00;
    memory[763]=0x50;
    memory[764]=0xFF;
    memory[765]=0x15;
    memory[766]=0x0C;
    memory[767]=0x10;
    memory[768]=0x40;
    memory[769]=0x00;
    memory[770]=0x09;
    memory[771]=0xC0;
    memory[772]=0x75;
    memory[773]=0x08;
    memory[774]=0x6A;
    memory[775]=0x00;
    memory[776]=0xFF;
    memory[777]=0x15;
    memory[778]=0x00;
    memory[779]=0x10;
    memory[780]=0x40;
    memory[781]=0x00;
    memory[782]=0x81;
    memory[783]=0x3D;
    memory[784]=0x30;
    memory[785]=0x11;
    memory[786]=0x40;
    memory[787]=0x00;
    memory[788]=0x01;
    memory[789]=0x00;
    memory[790]=0x00;
    memory[791]=0x00;
    memory[792]=0x75;
    memory[793]=0xEC;
    memory[794]=0x58;
    memory[795]=0xC3;
    memory[796]=0x00;
    memory[797]=0x57;
    memory[798]=0x72;
    memory[799]=0x69;
    memory[800]=0x74;
    memory[801]=0x65;
    memory[802]=0x20;
    memory[803]=0x65;
    memory[804]=0x72;
    memory[805]=0x72;
    memory[806]=0x6F;
    memory[807]=0x72;
    memory[808]=0x00;
    memory[809]=0x00;
    memory[810]=0x00;
    memory[811]=0x00;
    memory[812]=0x00;
    memory[813]=0x00;
    memory[814]=0x00;
    memory[815]=0x00;
    memory[816]=0x00;
    memory[817]=0x00;
    memory[818]=0x00;
    memory[819]=0x00;
    memory[820]=0x00;
    memory[821]=0x00;
    memory[822]=0x00;
    memory[823]=0x00;
    memory[824]=0x00;
    memory[825]=0x00;
    memory[826]=0x00;
    memory[827]=0x00;
    memory[828]=0x00;
    memory[829]=0x00;
    memory[830]=0x00;
    memory[831]=0x00;
    memory[832]=0x60;
    memory[833]=0x31;
    memory[834]=0xC0;
    memory[835]=0x03;
    memory[836]=0x05;
    memory[837]=0xCC;
    memory[838]=0x11;
    memory[839]=0x40;
    memory[840]=0x00;
    memory[841]=0x75;
    memory[842]=0x37;
    memory[843]=0x6A;
    memory[844]=0xF6;
    memory[845]=0xFF;
    memory[846]=0x15;
    memory[847]=0x04;
    memory[848]=0x10;
    memory[849]=0x40;
    memory[850]=0x00;
    memory[851]=0xA3;
    memory[852]=0xCC;
    memory[853]=0x11;
    memory[854]=0x40;
    memory[855]=0x00;
    memory[856]=0x68;
    memory[857]=0xD0;
    memory[858]=0x11;
    memory[859]=0x40;
    memory[860]=0x00;
    memory[861]=0x50;
    memory[862]=0xFF;
    memory[863]=0x15;
    memory[864]=0x10;
    memory[865]=0x10;
    memory[866]=0x40;
    memory[867]=0x00;
    memory[868]=0x80;
    memory[869]=0x25;
    memory[870]=0xD0;
    memory[871]=0x11;
    memory[872]=0x40;
    memory[873]=0x00;
    memory[874]=0xF9;
    memory[875]=0xFF;
    memory[876]=0x35;
    memory[877]=0xD0;
    memory[878]=0x11;
    memory[879]=0x40;
    memory[880]=0x00;
    memory[881]=0xFF;
    memory[882]=0x35;
    memory[883]=0xCC;
    memory[884]=0x11;
    memory[885]=0x40;
    memory[886]=0x00;
    memory[887]=0xFF;
    memory[888]=0x15;
    memory[889]=0x14;
    memory[890]=0x10;
    memory[891]=0x40;
    memory[892]=0x00;
    memory[893]=0xA1;
    memory[894]=0xCC;
    memory[895]=0x11;
    memory[896]=0x40;
    memory[897]=0x00;
    memory[898]=0x6A;
    memory[899]=0x00;
    memory[900]=0x68;
    memory[901]=0xD4;
    memory[902]=0x11;
    memory[903]=0x40;
    memory[904]=0x00;
    memory[905]=0x6A;
    memory[906]=0x01;
    memory[907]=0x68;
    memory[908]=0xBE;
    memory[909]=0x11;
    memory[910]=0x40;
    memory[911]=0x00;
    memory[912]=0x50;
    memory[913]=0xFF;
    memory[914]=0x15;
    memory[915]=0x08;
    memory[916]=0x10;
    memory[917]=0x40;
    memory[918]=0x00;
    memory[919]=0x09;
    memory[920]=0xC0;
    memory[921]=0x61;
    memory[922]=0x90;
    memory[923]=0x75;
    memory[924]=0x08;
    memory[925]=0x6A;
    memory[926]=0x00;
    memory[927]=0xFF;
    memory[928]=0x15;
    memory[929]=0x00;
    memory[930]=0x10;
    memory[931]=0x40;
    memory[932]=0x00;
    memory[933]=0x0F;
    memory[934]=0xB6;
    memory[935]=0x05;
    memory[936]=0xBE;
    memory[937]=0x11;
    memory[938]=0x40;
    memory[939]=0x00;
    memory[940]=0x81;
    memory[941]=0x3D;
    memory[942]=0xD4;
    memory[943]=0x11;
    memory[944]=0x40;
    memory[945]=0x00;
    memory[946]=0x01;
    memory[947]=0x00;
    memory[948]=0x00;
    memory[949]=0x00;
    memory[950]=0x74;
    memory[951]=0x05;
    memory[952]=0xB8;
    memory[953]=0xFF;
    memory[954]=0xFF;
    memory[955]=0xFF;
    memory[956]=0xFF;
    memory[957]=0xC3;
    memory[958]=0x00;
    memory[959]=0x52;
    memory[960]=0x65;
    memory[961]=0x61;
    memory[962]=0x64;
    memory[963]=0x20;
    memory[964]=0x65;
    memory[965]=0x72;
    memory[966]=0x72;
    memory[967]=0x6F;
    memory[968]=0x72;
    memory[969]=0x00;
    memory[970]=0x00;
    memory[971]=0x00;
    memory[972]=0x00;
    memory[973]=0x00;
    memory[974]=0x00;
    memory[975]=0x00;
    memory[976]=0x00;
    memory[977]=0x00;
    memory[978]=0x00;
    memory[979]=0x00;
    memory[980]=0x00;
    memory[981]=0x00;
    memory[982]=0x00;
    memory[983]=0x00;
    memory[984]=0x00;
    memory[985]=0x00;
    memory[986]=0x00;
    memory[987]=0x00;
    memory[988]=0x00;
    memory[989]=0x00;
    memory[990]=0x00;
    memory[991]=0x00;
    memory[992]=0x60;
    memory[993]=0x89;
    memory[994]=0xC6;
    memory[995]=0x30;
    memory[996]=0xC0;
    memory[997]=0x02;
    memory[998]=0x06;
    memory[999]=0x74;
    memory[1000]=0x08;
    memory[1001]=0x46;
    memory[1002]=0xE8;
    memory[1003]=0xE1;
    memory[1004]=0xFE;
    memory[1005]=0xFF;
    memory[1006]=0xFF;
    memory[1007]=0xEB;
    memory[1008]=0xF2;
    memory[1009]=0x61;
    memory[1010]=0x90;
    memory[1011]=0xC3;
    memory[1012]=0x00;
    memory[1013]=0x00;
    memory[1014]=0x00;
    memory[1015]=0x00;
    memory[1016]=0x00;
    memory[1017]=0x00;
    memory[1018]=0x00;
    memory[1019]=0x00;
    memory[1020]=0x00;
    memory[1021]=0x00;
    memory[1022]=0x00;
    memory[1023]=0x00;
    memory[1024]=0x04;
    memory[1025]=0x30;
    memory[1026]=0xE8;
    memory[1027]=0xC9;
    memory[1028]=0xFE;
    memory[1029]=0xFF;
    memory[1030]=0xFF;
    memory[1031]=0xC3;
    memory[1032]=0x00;
    memory[1033]=0x00;
    memory[1034]=0x00;
    memory[1035]=0x00;
    memory[1036]=0x00;
    memory[1037]=0x00;
    memory[1038]=0x00;
    memory[1039]=0x00;
    memory[1040]=0xB0;
    memory[1041]=0x0D;
    memory[1042]=0xE8;
    memory[1043]=0xB9;
    memory[1044]=0xFE;
    memory[1045]=0xFF;
    memory[1046]=0xFF;
    memory[1047]=0xB0;
    memory[1048]=0x0A;
    memory[1049]=0xE8;
    memory[1050]=0xB2;
    memory[1051]=0xFE;
    memory[1052]=0xFF;
    memory[1053]=0xFF;
    memory[1054]=0xC3;
    memory[1055]=0x00;
    memory[1056]=0x3D;
    memory[1057]=0x00;
    memory[1058]=0x00;
    memory[1059]=0x00;
    memory[1060]=0x80;
    memory[1061]=0x75;
    memory[1062]=0x4E;
    memory[1063]=0xB0;
    memory[1064]=0x2D;
    memory[1065]=0xE8;
    memory[1066]=0xA2;
    memory[1067]=0xFE;
    memory[1068]=0xFF;
    memory[1069]=0xFF;
    memory[1070]=0xB0;
    memory[1071]=0x02;
    memory[1072]=0xE8;
    memory[1073]=0xCB;
    memory[1074]=0xFF;
    memory[1075]=0xFF;
    memory[1076]=0xFF;
    memory[1077]=0xB0;
    memory[1078]=0x01;
    memory[1079]=0xE8;
    memory[1080]=0xC4;
    memory[1081]=0xFF;
    memory[1082]=0xFF;
    memory[1083]=0xFF;
    memory[1084]=0xB0;
    memory[1085]=0x04;
    memory[1086]=0xE8;
    memory[1087]=0xBD;
    memory[1088]=0xFF;
    memory[1089]=0xFF;
    memory[1090]=0xFF;
    memory[1091]=0xB0;
    memory[1092]=0x07;
    memory[1093]=0xE8;
    memory[1094]=0xB6;
    memory[1095]=0xFF;
    memory[1096]=0xFF;
    memory[1097]=0xFF;
    memory[1098]=0xB0;
    memory[1099]=0x04;
    memory[1100]=0xE8;
    memory[1101]=0xAF;
    memory[1102]=0xFF;
    memory[1103]=0xFF;
    memory[1104]=0xFF;
    memory[1105]=0xB0;
    memory[1106]=0x08;
    memory[1107]=0xE8;
    memory[1108]=0xA8;
    memory[1109]=0xFF;
    memory[1110]=0xFF;
    memory[1111]=0xFF;
    memory[1112]=0xB0;
    memory[1113]=0x03;
    memory[1114]=0xE8;
    memory[1115]=0xA1;
    memory[1116]=0xFF;
    memory[1117]=0xFF;
    memory[1118]=0xFF;
    memory[1119]=0xB0;
    memory[1120]=0x06;
    memory[1121]=0xE8;
    memory[1122]=0x9A;
    memory[1123]=0xFF;
    memory[1124]=0xFF;
    memory[1125]=0xFF;
    memory[1126]=0xB0;
    memory[1127]=0x04;
    memory[1128]=0xE8;
    memory[1129]=0x93;
    memory[1130]=0xFF;
    memory[1131]=0xFF;
    memory[1132]=0xFF;
    memory[1133]=0xB0;
    memory[1134]=0x08;
    memory[1135]=0xE8;
    memory[1136]=0x8C;
    memory[1137]=0xFF;
    memory[1138]=0xFF;
    memory[1139]=0xFF;
    memory[1140]=0xC3;
    memory[1141]=0x3D;
    memory[1142]=0x00;
    memory[1143]=0x00;
    memory[1144]=0x00;
    memory[1145]=0x00;
    memory[1146]=0x7D;
    memory[1147]=0x0B;
    memory[1148]=0x50;
    memory[1149]=0xB0;
    memory[1150]=0x2D;
    memory[1151]=0xE8;
    memory[1152]=0x4C;
    memory[1153]=0xFE;
    memory[1154]=0xFF;
    memory[1155]=0xFF;
    memory[1156]=0x58;
    memory[1157]=0xF7;
    memory[1158]=0xD8;
    memory[1159]=0x3D;
    memory[1160]=0x0A;
    memory[1161]=0x00;
    memory[1162]=0x00;
    memory[1163]=0x00;
    memory[1164]=0x0F;
    memory[1165]=0x8C;
    memory[1166]=0xEF;
    memory[1167]=0x00;
    memory[1168]=0x00;
    memory[1169]=0x00;
    memory[1170]=0x3D;
    memory[1171]=0x64;
    memory[1172]=0x00;
    memory[1173]=0x00;
    memory[1174]=0x00;
    memory[1175]=0x0F;
    memory[1176]=0x8C;
    memory[1177]=0xD1;
    memory[1178]=0x00;
    memory[1179]=0x00;
    memory[1180]=0x00;
    memory[1181]=0x3D;
    memory[1182]=0xE8;
    memory[1183]=0x03;
    memory[1184]=0x00;
    memory[1185]=0x00;
    memory[1186]=0x0F;
    memory[1187]=0x8C;
    memory[1188]=0xB3;
    memory[1189]=0x00;
    memory[1190]=0x00;
    memory[1191]=0x00;
    memory[1192]=0x3D;
    memory[1193]=0x10;
    memory[1194]=0x27;
    memory[1195]=0x00;
    memory[1196]=0x00;
    memory[1197]=0x0F;
    memory[1198]=0x8C;
    memory[1199]=0x95;
    memory[1200]=0x00;
    memory[1201]=0x00;
    memory[1202]=0x00;
    memory[1203]=0x3D;
    memory[1204]=0xA0;
    memory[1205]=0x86;
    memory[1206]=0x01;
    memory[1207]=0x00;
    memory[1208]=0x7C;
    memory[1209]=0x7B;
    memory[1210]=0x3D;
    memory[1211]=0x40;
    memory[1212]=0x42;
    memory[1213]=0x0F;
    memory[1214]=0x00;
    memory[1215]=0x7C;
    memory[1216]=0x61;
    memory[1217]=0x3D;
    memory[1218]=0x80;
    memory[1219]=0x96;
    memory[1220]=0x98;
    memory[1221]=0x00;
    memory[1222]=0x7C;
    memory[1223]=0x47;
    memory[1224]=0x3D;
    memory[1225]=0x00;
    memory[1226]=0xE1;
    memory[1227]=0xF5;
    memory[1228]=0x05;
    memory[1229]=0x7C;
    memory[1230]=0x2D;
    memory[1231]=0x3D;
    memory[1232]=0x00;
    memory[1233]=0xCA;
    memory[1234]=0x9A;
    memory[1235]=0x3B;
    memory[1236]=0x7C;
    memory[1237]=0x13;
    memory[1238]=0xBA;
    memory[1239]=0x00;
    memory[1240]=0x00;
    memory[1241]=0x00;
    memory[1242]=0x00;
    memory[1243]=0xBB;
    memory[1244]=0x00;
    memory[1245]=0xCA;
    memory[1246]=0x9A;
    memory[1247]=0x3B;
    memory[1248]=0xF7;
    memory[1249]=0xFB;
    memory[1250]=0x52;
    memory[1251]=0xE8;
    memory[1252]=0x18;
    memory[1253]=0xFF;
    memory[1254]=0xFF;
    memory[1255]=0xFF;
    memory[1256]=0x58;
    memory[1257]=0xBA;
    memory[1258]=0x00;
    memory[1259]=0x00;
    memory[1260]=0x00;
    memory[1261]=0x00;
    memory[1262]=0xBB;
    memory[1263]=0x00;
    memory[1264]=0xE1;
    memory[1265]=0xF5;
    memory[1266]=0x05;
    memory[1267]=0xF7;
    memory[1268]=0xFB;
    memory[1269]=0x52;
    memory[1270]=0xE8;
    memory[1271]=0x05;
    memory[1272]=0xFF;
    memory[1273]=0xFF;
    memory[1274]=0xFF;
    memory[1275]=0x58;
    memory[1276]=0xBA;
    memory[1277]=0x00;
    memory[1278]=0x00;
    memory[1279]=0x00;
    memory[1280]=0x00;
    memory[1281]=0xBB;
    memory[1282]=0x80;
    memory[1283]=0x96;
    memory[1284]=0x98;
    memory[1285]=0x00;
    memory[1286]=0xF7;
    memory[1287]=0xFB;
    memory[1288]=0x52;
    memory[1289]=0xE8;
    memory[1290]=0xF2;
    memory[1291]=0xFE;
    memory[1292]=0xFF;
    memory[1293]=0xFF;
    memory[1294]=0x58;
    memory[1295]=0xBA;
    memory[1296]=0x00;
    memory[1297]=0x00;
    memory[1298]=0x00;
    memory[1299]=0x00;
    memory[1300]=0xBB;
    memory[1301]=0x40;
    memory[1302]=0x42;
    memory[1303]=0x0F;
    memory[1304]=0x00;
    memory[1305]=0xF7;
    memory[1306]=0xFB;
    memory[1307]=0x52;
    memory[1308]=0xE8;
    memory[1309]=0xDF;
    memory[1310]=0xFE;
    memory[1311]=0xFF;
    memory[1312]=0xFF;
    memory[1313]=0x58;
    memory[1314]=0xBA;
    memory[1315]=0x00;
    memory[1316]=0x00;
    memory[1317]=0x00;
    memory[1318]=0x00;
    memory[1319]=0xBB;
    memory[1320]=0xA0;
    memory[1321]=0x86;
    memory[1322]=0x01;
    memory[1323]=0x00;
    memory[1324]=0xF7;
    memory[1325]=0xFB;
    memory[1326]=0x52;
    memory[1327]=0xE8;
    memory[1328]=0xCC;
    memory[1329]=0xFE;
    memory[1330]=0xFF;
    memory[1331]=0xFF;
    memory[1332]=0x58;
    memory[1333]=0xBA;
    memory[1334]=0x00;
    memory[1335]=0x00;
    memory[1336]=0x00;
    memory[1337]=0x00;
    memory[1338]=0xBB;
    memory[1339]=0x10;
    memory[1340]=0x27;
    memory[1341]=0x00;
    memory[1342]=0x00;
    memory[1343]=0xF7;
    memory[1344]=0xFB;
    memory[1345]=0x52;
    memory[1346]=0xE8;
    memory[1347]=0xB9;
    memory[1348]=0xFE;
    memory[1349]=0xFF;
    memory[1350]=0xFF;
    memory[1351]=0x58;
    memory[1352]=0xBA;
    memory[1353]=0x00;
    memory[1354]=0x00;
    memory[1355]=0x00;
    memory[1356]=0x00;
    memory[1357]=0xBB;
    memory[1358]=0xE8;
    memory[1359]=0x03;
    memory[1360]=0x00;
    memory[1361]=0x00;
    memory[1362]=0xF7;
    memory[1363]=0xFB;
    memory[1364]=0x52;
    memory[1365]=0xE8;
    memory[1366]=0xA6;
    memory[1367]=0xFE;
    memory[1368]=0xFF;
    memory[1369]=0xFF;
    memory[1370]=0x58;
    memory[1371]=0xBA;
    memory[1372]=0x00;
    memory[1373]=0x00;
    memory[1374]=0x00;
    memory[1375]=0x00;
    memory[1376]=0xBB;
    memory[1377]=0x64;
    memory[1378]=0x00;
    memory[1379]=0x00;
    memory[1380]=0x00;
    memory[1381]=0xF7;
    memory[1382]=0xFB;
    memory[1383]=0x52;
    memory[1384]=0xE8;
    memory[1385]=0x93;
    memory[1386]=0xFE;
    memory[1387]=0xFF;
    memory[1388]=0xFF;
    memory[1389]=0x58;
    memory[1390]=0xBA;
    memory[1391]=0x00;
    memory[1392]=0x00;
    memory[1393]=0x00;
    memory[1394]=0x00;
    memory[1395]=0xBB;
    memory[1396]=0x0A;
    memory[1397]=0x00;
    memory[1398]=0x00;
    memory[1399]=0x00;
    memory[1400]=0xF7;
    memory[1401]=0xFB;
    memory[1402]=0x52;
    memory[1403]=0xE8;
    memory[1404]=0x80;
    memory[1405]=0xFE;
    memory[1406]=0xFF;
    memory[1407]=0xFF;
    memory[1408]=0x58;
    memory[1409]=0xE8;
    memory[1410]=0x7A;
    memory[1411]=0xFE;
    memory[1412]=0xFF;
    memory[1413]=0xFF;
    memory[1414]=0xC3;
    memory[1415]=0x00;
    memory[1416]=0xFF;
    memory[1417]=0x15;
    memory[1418]=0x00;
    memory[1419]=0x10;
    memory[1420]=0x40;
    memory[1421]=0x00;
    memory[1422]=0x00;
    memory[1423]=0x00;
    memory[1424]=0xB9;
    memory[1425]=0x00;
    memory[1426]=0x00;
    memory[1427]=0x00;
    memory[1428]=0x00;
    memory[1429]=0xB3;
    memory[1430]=0x03;
    memory[1431]=0x51;
    memory[1432]=0x53;
    memory[1433]=0xE8;
    memory[1434]=0xA2;
    memory[1435]=0xFD;
    memory[1436]=0xFF;
    memory[1437]=0xFF;
    memory[1438]=0x5B;
    memory[1439]=0x59;
    memory[1440]=0x3C;
    memory[1441]=0x0D;
    memory[1442]=0x0F;
    memory[1443]=0x84;
    memory[1444]=0x34;
    memory[1445]=0x01;
    memory[1446]=0x00;
    memory[1447]=0x00;
    memory[1448]=0x3C;
    memory[1449]=0x08;
    memory[1450]=0x0F;
    memory[1451]=0x84;
    memory[1452]=0x94;
    memory[1453]=0x00;
    memory[1454]=0x00;
    memory[1455]=0x00;
    memory[1456]=0x3C;
    memory[1457]=0x2D;
    memory[1458]=0x0F;
    memory[1459]=0x84;
    memory[1460]=0x09;
    memory[1461]=0x01;
    memory[1462]=0x00;
    memory[1463]=0x00;
    memory[1464]=0x3C;
    memory[1465]=0x30;
    memory[1466]=0x7C;
    memory[1467]=0xDB;
    memory[1468]=0x3C;
    memory[1469]=0x39;
    memory[1470]=0x7F;
    memory[1471]=0xD7;
    memory[1472]=0x2C;
    memory[1473]=0x30;
    memory[1474]=0x80;
    memory[1475]=0xFB;
    memory[1476]=0x00;
    memory[1477]=0x74;
    memory[1478]=0xD0;
    memory[1479]=0x80;
    memory[1480]=0xFB;
    memory[1481]=0x02;
    memory[1482]=0x75;
    memory[1483]=0x0C;
    memory[1484]=0x81;
    memory[1485]=0xF9;
    memory[1486]=0x00;
    memory[1487]=0x00;
    memory[1488]=0x00;
    memory[1489]=0x00;
    memory[1490]=0x75;
    memory[1491]=0x04;
    memory[1492]=0x3C;
    memory[1493]=0x00;
    memory[1494]=0x74;
    memory[1495]=0xBF;
    memory[1496]=0x80;
    memory[1497]=0xFB;
    memory[1498]=0x03;
    memory[1499]=0x75;
    memory[1500]=0x0A;
    memory[1501]=0x3C;
    memory[1502]=0x00;
    memory[1503]=0x75;
    memory[1504]=0x04;
    memory[1505]=0xB3;
    memory[1506]=0x00;
    memory[1507]=0xEB;
    memory[1508]=0x02;
    memory[1509]=0xB3;
    memory[1510]=0x01;
    memory[1511]=0x81;
    memory[1512]=0xF9;
    memory[1513]=0xCC;
    memory[1514]=0xCC;
    memory[1515]=0xCC;
    memory[1516]=0x0C;
    memory[1517]=0x7F;
    memory[1518]=0xA8;
    memory[1519]=0x81;
    memory[1520]=0xF9;
    memory[1521]=0x34;
    memory[1522]=0x33;
    memory[1523]=0x33;
    memory[1524]=0xF3;
    memory[1525]=0x7C;
    memory[1526]=0xA0;
    memory[1527]=0x88;
    memory[1528]=0xC7;
    memory[1529]=0xB8;
    memory[1530]=0x0A;
    memory[1531]=0x00;
    memory[1532]=0x00;
    memory[1533]=0x00;
    memory[1534]=0xF7;
    memory[1535]=0xE9;
    memory[1536]=0x3D;
    memory[1537]=0x08;
    memory[1538]=0x00;
    memory[1539]=0x00;
    memory[1540]=0x80;
    memory[1541]=0x74;
    memory[1542]=0x11;
    memory[1543]=0x3D;
    memory[1544]=0xF8;
    memory[1545]=0xFF;
    memory[1546]=0xFF;
    memory[1547]=0x7F;
    memory[1548]=0x75;
    memory[1549]=0x13;
    memory[1550]=0x80;
    memory[1551]=0xFF;
    memory[1552]=0x07;
    memory[1553]=0x7E;
    memory[1554]=0x0E;
    memory[1555]=0xE9;
    memory[1556]=0x7F;
    memory[1557]=0xFF;
    memory[1558]=0xFF;
    memory[1559]=0xFF;
    memory[1560]=0x80;
    memory[1561]=0xFF;
    memory[1562]=0x08;
    memory[1563]=0x0F;
    memory[1564]=0x8F;
    memory[1565]=0x76;
    memory[1566]=0xFF;
    memory[1567]=0xFF;
    memory[1568]=0xFF;
    memory[1569]=0xB9;
    memory[1570]=0x00;
    memory[1571]=0x00;
    memory[1572]=0x00;
    memory[1573]=0x00;
    memory[1574]=0x88;
    memory[1575]=0xF9;
    memory[1576]=0x80;
    memory[1577]=0xFB;
    memory[1578]=0x02;
    memory[1579]=0x74;
    memory[1580]=0x04;
    memory[1581]=0x01;
    memory[1582]=0xC1;
    memory[1583]=0xEB;
    memory[1584]=0x03;
    memory[1585]=0x29;
    memory[1586]=0xC8;
    memory[1587]=0x91;
    memory[1588]=0x88;
    memory[1589]=0xF8;
    memory[1590]=0x51;
    memory[1591]=0x53;
    memory[1592]=0xE8;
    memory[1593]=0xC3;
    memory[1594]=0xFD;
    memory[1595]=0xFF;
    memory[1596]=0xFF;
    memory[1597]=0x5B;
    memory[1598]=0x59;
    memory[1599]=0xE9;
    memory[1600]=0x53;
    memory[1601]=0xFF;
    memory[1602]=0xFF;
    memory[1603]=0xFF;
    memory[1604]=0x80;
    memory[1605]=0xFB;
    memory[1606]=0x03;
    memory[1607]=0x0F;
    memory[1608]=0x84;
    memory[1609]=0x4A;
    memory[1610]=0xFF;
    memory[1611]=0xFF;
    memory[1612]=0xFF;
    memory[1613]=0x51;
    memory[1614]=0x53;
    memory[1615]=0xB0;
    memory[1616]=0x08;
    memory[1617]=0xE8;
    memory[1618]=0x7A;
    memory[1619]=0xFC;
    memory[1620]=0xFF;
    memory[1621]=0xFF;
    memory[1622]=0xB0;
    memory[1623]=0x20;
    memory[1624]=0xE8;
    memory[1625]=0x73;
    memory[1626]=0xFC;
    memory[1627]=0xFF;
    memory[1628]=0xFF;
    memory[1629]=0xB0;
    memory[1630]=0x08;
    memory[1631]=0xE8;
    memory[1632]=0x6C;
    memory[1633]=0xFC;
    memory[1634]=0xFF;
    memory[1635]=0xFF;
    memory[1636]=0x5B;
    memory[1637]=0x59;
    memory[1638]=0x80;
    memory[1639]=0xFB;
    memory[1640]=0x00;
    memory[1641]=0x75;
    memory[1642]=0x07;
    memory[1643]=0xB3;
    memory[1644]=0x03;
    memory[1645]=0xE9;
    memory[1646]=0x25;
    memory[1647]=0xFF;
    memory[1648]=0xFF;
    memory[1649]=0xFF;
    memory[1650]=0x80;
    memory[1651]=0xFB;
    memory[1652]=0x02;
    memory[1653]=0x75;
    memory[1654]=0x0F;
    memory[1655]=0x81;
    memory[1656]=0xF9;
    memory[1657]=0x00;
    memory[1658]=0x00;
    memory[1659]=0x00;
    memory[1660]=0x00;
    memory[1661]=0x75;
    memory[1662]=0x07;
    memory[1663]=0xB3;
    memory[1664]=0x03;
    memory[1665]=0xE9;
    memory[1666]=0x11;
    memory[1667]=0xFF;
    memory[1668]=0xFF;
    memory[1669]=0xFF;
    memory[1670]=0x89;
    memory[1671]=0xC8;
    memory[1672]=0xB9;
    memory[1673]=0x0A;
    memory[1674]=0x00;
    memory[1675]=0x00;
    memory[1676]=0x00;
    memory[1677]=0xBA;
    memory[1678]=0x00;
    memory[1679]=0x00;
    memory[1680]=0x00;
    memory[1681]=0x00;
    memory[1682]=0x3D;
    memory[1683]=0x00;
    memory[1684]=0x00;
    memory[1685]=0x00;
    memory[1686]=0x00;
    memory[1687]=0x7D;
    memory[1688]=0x08;
    memory[1689]=0xF7;
    memory[1690]=0xD8;
    memory[1691]=0xF7;
    memory[1692]=0xF9;
    memory[1693]=0xF7;
    memory[1694]=0xD8;
    memory[1695]=0xEB;
    memory[1696]=0x02;
    memory[1697]=0xF7;
    memory[1698]=0xF9;
    memory[1699]=0x89;
    memory[1700]=0xC1;
    memory[1701]=0x81;
    memory[1702]=0xF9;
    memory[1703]=0x00;
    memory[1704]=0x00;
    memory[1705]=0x00;
    memory[1706]=0x00;
    memory[1707]=0x0F;
    memory[1708]=0x85;
    memory[1709]=0xE6;
    memory[1710]=0xFE;
    memory[1711]=0xFF;
    memory[1712]=0xFF;
    memory[1713]=0x80;
    memory[1714]=0xFB;
    memory[1715]=0x02;
    memory[1716]=0x0F;
    memory[1717]=0x84;
    memory[1718]=0xDD;
    memory[1719]=0xFE;
    memory[1720]=0xFF;
    memory[1721]=0xFF;
    memory[1722]=0xB3;
    memory[1723]=0x03;
    memory[1724]=0xE9;
    memory[1725]=0xD6;
    memory[1726]=0xFE;
    memory[1727]=0xFF;
    memory[1728]=0xFF;
    memory[1729]=0x80;
    memory[1730]=0xFB;
    memory[1731]=0x03;
    memory[1732]=0x0F;
    memory[1733]=0x85;
    memory[1734]=0xCD;
    memory[1735]=0xFE;
    memory[1736]=0xFF;
    memory[1737]=0xFF;
    memory[1738]=0xB0;
    memory[1739]=0x2D;
    memory[1740]=0x51;
    memory[1741]=0x53;
    memory[1742]=0xE8;
    memory[1743]=0xFD;
    memory[1744]=0xFB;
    memory[1745]=0xFF;
    memory[1746]=0xFF;
    memory[1747]=0x5B;
    memory[1748]=0x59;
    memory[1749]=0xB3;
    memory[1750]=0x02;
    memory[1751]=0xE9;
    memory[1752]=0xBB;
    memory[1753]=0xFE;
    memory[1754]=0xFF;
    memory[1755]=0xFF;
    memory[1756]=0x80;
    memory[1757]=0xFB;
    memory[1758]=0x03;
    memory[1759]=0x0F;
    memory[1760]=0x84;
    memory[1761]=0xB2;
    memory[1762]=0xFE;
    memory[1763]=0xFF;
    memory[1764]=0xFF;
    memory[1765]=0x80;
    memory[1766]=0xFB;
    memory[1767]=0x02;
    memory[1768]=0x75;
    memory[1769]=0x0C;
    memory[1770]=0x81;
    memory[1771]=0xF9;
    memory[1772]=0x00;
    memory[1773]=0x00;
    memory[1774]=0x00;
    memory[1775]=0x00;
    memory[1776]=0x0F;
    memory[1777]=0x84;
    memory[1778]=0xA1;
    memory[1779]=0xFE;
    memory[1780]=0xFF;
    memory[1781]=0xFF;
    memory[1782]=0x51;
    memory[1783]=0xE8;
    memory[1784]=0x14;
    memory[1785]=0xFD;
    memory[1786]=0xFF;
    memory[1787]=0xFF;
    memory[1788]=0x59;
    memory[1789]=0x89;
    memory[1790]=0xC8;
    memory[1791]=0xC3;

    *memoryLimit = 1792;
    int variablesCounter = 0;
    generateByte(0xBF, memory, memoryLimit); // MOV EDI, ...
    generateInt(0, memory, memoryLimit);
    block(file, errorFile, errorFileName, memory, memoryLimit, &variablesCounter, str, remaining, symbol, lineCount, 0, table);
    if (*symbol == POINT) {
        getSymbol(file, errorFile, str, remaining, symbol, lineCount);
    } else {
        showErrorCode(file, errorFile, errorFileName, 2, str);
    }
    
    generateByte(0xE9, memory, memoryLimit); // Jump backwards(0x588).
    int distance = 0x588 - ((*memoryLimit) + 4); // 0x588 ends the program.
    generateInt(distance, memory, memoryLimit);
    const int  headerSizeInHexadecimal = 0x200;
    int variablesStart = 0x401000 + (*memoryLimit - headerSizeInHexadecimal);
    generateIntIn(variablesStart, 1793, memory);
    for (int i = 0; i < variablesCounter; i++) {
        generateInt(0, memory, memoryLimit);
    }
    
    int currentTextSectionSize = *memoryLimit - headerSizeInHexadecimal;
    generateIntIn(currentTextSectionSize, 0x01A0, memory);
    int fileAlignment = readIntSince(0x00DC, memory);
    while (*memoryLimit % fileAlignment != 0) {
        generateByte(0, memory, memoryLimit);
    }
    
    currentTextSectionSize = *memoryLimit - headerSizeInHexadecimal;
    generateIntIn(currentTextSectionSize, 0x00BC, memory);
    generateIntIn(currentTextSectionSize, 0x01A8, memory);
    int sizeOfCodeSection = readIntSince(0x00BC, memory);
    int sectionAlignment = readIntSince(0x00D8, memory);
    int sizeOfRawData = readIntSince(0x01A8, memory);
    generateIntIn((2 + sizeOfCodeSection / sectionAlignment) * sectionAlignment, 0x00F0, memory);
    generateIntIn((2 + sizeOfRawData / sectionAlignment) * sectionAlignment, 0x00D0, memory);
}

void changeExtentionToExe(string readFileName, string* executeFileName) { 
   
   char* token = strtok(readFileName, ".");
   while( token != NULL ) {
      strcat(*(executeFileName), token);
      strcat(*(executeFileName), ".exe");
      token =  NULL;
   }
}

int main(int argc, char *argv[])
{
    FILE *file;
    FILE *errorFile;
    FILE *executeFile;
    string readFileName = "";
    string errorFileName = "ERROR-";
    string executeFileName = "";
    string remaining = "";
    string str = "";
    symbol_t symbol;
    identifierTable table;
    vectorBytes_t memory;
    int lineCount = 1;
    int memoryLimit = 0;

    if (argc != 2) {
        printf("Ingrese el nombre del archivo a abrir\n");
        gets(readFileName);
        file = fopen(readFileName, "rt");
        strcat(errorFileName, readFileName);
        errorFile = fopen(errorFileName, "w");
    } else {
        file = fopen(argv[1], "rt");
        strcat(readFileName, argv[1]);
        strcat(errorFileName, argv[1]);
        errorFile = fopen(errorFileName, "w");
    }
    fgets(remaining,500, file);
    if (remaining != NULL) { 
        fprintf(errorFile, "%3d: %s", lineCount, remaining);
        lineCount++;
    }

    getSymbol(file, errorFile, str, remaining, &symbol, &lineCount);
    program(file, errorFile, errorFileName, memory, &memoryLimit, str, remaining, &symbol, &lineCount, table);
    if (symbol != EOFILE){
        showErrorCode(file, errorFile, errorFileName, 1, NULL);
    } else {
        changeExtentionToExe(readFileName, &executeFileName);
        executeFile = fopen(executeFileName, "wb");
        fwrite(memory, sizeof(unsigned char), memoryLimit, executeFile);
    }
    fclose(executeFile);
    showErrorCode(file, errorFile, errorFileName, 0, NULL);          
    system("pause");
    return 0;
}
