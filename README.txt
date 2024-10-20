# OCR-Pseudocode
OCR Pseudocode Interpreter  

Usage: This version of OCR.py currently works on a text file called "test.txt" (containing the OCR Pseudocode), which must exist in the same directory. This can easily be changed by altering the text file name at the bottom of OCR.py. Also note, there must be at least 1 newline at the end of the file.

Link to OCR Pseudocode Syntax: https://www.ocr.org.uk/Images/202654-pseudocode-guide.pdf
Not Supported: string.length, string.substring, Arrays, File Handling, Comments

Data types: Integer, Float, Boolean (True/False), String (Enclosed by Double Quotes Only)
Arithmetic Operators:
+ (Add), - (Subtract), * (Multiply), / (Divide), MOD (Remainder), DIV (Integer Division), ^ (Exponentiation)
Logical Operators: AND, OR, NOT
Comparison Operators: == (Equal), != (Not Equal), < (Less Than), <= (Less Than/Equal to), > (Greater Than), >= (Greater Than/Equal to)

Indexing Begins at 0  
Increment by, for example, var = var + 1  
  
Built-in Functions: 
input(OPTIONAL PROMPT)              (Console Input)
int(INT/FLOAT/STRING)               (Converts to Integer)
float(INT/FLOAT/STRING)             (Converts to Float)
str(INT/FLOAT/STRING/BOOLEAN)       (Converts to String)
bool(STRING/BOOLEAN)                (Converts to Boolean)
len(STRING)                         (Returns Number of Characters)
charAt(STRING, INTEGER)             (Returns Character at Index)

Built-in Procedures:
print(para1, para2..., paraN)       (Prints Arguments to Console Without Spaces Between)

Functions Called by functionName(para1, para2... paraN). Returns value.
Procedures Called by procedureName(para1, para2... paraN). Does not return value, must not be surrounded by other text in the same line.

Statement Syntax:

Example ASSIGNMENT STATEMENTS:

varOne = 103
varTwo = "Hello"
varThree = True
varFour = varThousand - 10

Example STRING CONCATENATION STATEMENTS:

varOne = "Hello " + "World"
varTwo = "Hello " + input("What is your name?")

Example IF STATEMENTS:

if entry == "a" then
  print("You selected a")
endif

if choice==1 then 
  print("Chose 1")
elseif choice==2 then
  print("Chose 2")
else if choice==3 then
  print("Chose 3")
else
  print("Okay then")
endif

Example SWITCH STATEMENTS:

switch entry:
  case "A":
    print("You selected A")
  case "B":
    print("You selected B")
  case 1:
    print("You selected INT 1")
  default:
    print("Unrecognised selection")
endswitch

Example FOR LOOPS: Upper and Lower Bound Are Included

for i = 0 to 7
  print("Hello " + str(i))
next i

Example WHILE LOOPS:

while answer != "computer"
  answer = input("What is the password?")
endwhile

while True
  print("Infinity")
endwhile

Example DO-UNTIL LOOPS:

do
  answer=input("What is the password?")
until answer=="computer"

do
  numVar = numVar + 1
until numVar == 10

Example FUNCTION DECLARATION:

function triple(number)
  return number * 3
endfunction

function processInput()
  number = int(input("Enter Number: "))
  while number > 10
    print("Error: Number Must Be Less Than 10")
    number = int(input("Enter Number: "))
  endwhile
  return number
endfunction

Example PROCEDURE DECLARATION:

procedure printTwice(statement)
  print(statement)
  print(statement)
endprocedure

