module TestUtils where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Parser qualified as P
import SQLSyntax
import TableParser qualified as TPAR
import TableSyntax

errorMsgUnitTest :: Either String b
errorMsgUnitTest = Left "No parses"

-------- Testing Support --------
tableSampleGradesTXT = "student_id,subject,grade\n1,Math,85\n1,English,78\n1,History,92\n2,English,88\n2,History,76\n3,Math,78"

tableSampleGradesPK = NE.fromList [(VarName "student_id", IntType 32), (VarName "subject", StringType 255)]

tableSampleGradesIN = [(VarName "grade", IntType 32)]

tableSampleGrades = case P.parse (TPAR.tableP tableSampleGradesPK tableSampleGradesIN) tableSampleGradesTXT of
  Right x -> x
  Left x -> Table tableSampleGradesPK tableSampleGradesIN []

-- >>> TP.pretty tableSampleGrades
-- "student_id,subject,grade\n1,Math,85\n1,English,78\n1,History,92\n2,English,88\n2,History,76\n3,Math,78"

tableSampleStudentsTXT = "student_id,first_name,last_name,gender,age\n1,John,Doe,Male,20\n2,Jane,Smith,Female,21\n4,Emily,Williams,Female,20"

tableSampleStudentsPK = NE.fromList [(VarName "student_id", IntType 32)]

tableSampleStudentsIN = [(VarName "first_name", StringType 255), (VarName "last_name", StringType 255), (VarName "gender", StringType 255), (VarName "age", IntType 32)]

tableSampleStudents = case P.parse (TPAR.tableP tableSampleStudentsPK tableSampleStudentsIN) tableSampleStudentsTXT of
  Right x -> x
  Left x -> Table tableSampleStudentsPK tableSampleStudentsIN []

minimumTableStudent = [(VarName "first_name", "")]

-- >>> TP.pretty tableSampleStudents
-- "student_id,first_name,last_name,gender,age\n1,John,Doe,Male,20\n2,Jane,Smith,Female,21\n3,Michael,Johnson,Male,22\n4,Emily,Williams,Female,20\n5,Chris,Anderson,Male,23"

tableNumber = Table (NE.fromList [(VarName "num1", IntType 32), (VarName "num2", IntType 32)]) [(VarName "num3", StringType 255)] [Map.fromList [(VarName "num1", IntVal 1), (VarName "num2", IntVal 1), (VarName "num3", StringVal "a")]]

-- >>> TP.pretty tableNumber
-- "num1,num2,num3\n1,1,a"

sampleStore :: Store
sampleStore = Store (Map.fromList [("Students", tableSampleStudents), ("Grades", tableSampleGrades), ("Number", tableNumber)]) Map.empty