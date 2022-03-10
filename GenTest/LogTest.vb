Imports System.Collections.Generic

Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for LogTest and is intended
'''to contain all LogTest Unit Tests
'''</summary>
<TestClass()> _
Public Class LogTest


    Private testContextInstance As TestContext

    '''<summary>
    '''Gets or sets the test context which provides
    '''information about and functionality for the current test run.
    '''</summary>
    Public Property TestContext() As TestContext
        Get
            Return testContextInstance
        End Get
        Set(value As TestContext)
            testContextInstance = Value
        End Set
    End Property

#Region "Additional test attributes"
    '
    'You can use the following additional attributes as you write your tests:
    '
    'Use ClassInitialize to run code before running the first test in the class
    '<ClassInitialize()>  _
    'Public Shared Sub MyClassInitialize(ByVal testContext As TestContext)
    'End Sub
    '
    'Use ClassCleanup to run code after all tests in a class have run
    '<ClassCleanup()>  _
    'Public Shared Sub MyClassCleanup()
    'End Sub
    '
    'Use TestInitialize to run code before running each test
    '<TestInitialize()>  _
    'Public Sub MyTestInitialize()
    'End Sub
    '
    'Use TestCleanup to run code after each test has run
    '<TestCleanup()>  _
    'Public Sub MyTestCleanup()
    'End Sub
    '
#End Region


    '''<summary>
    '''A test for LogPrint
    '''</summary>
    <TestMethod(), _
     DeploymentItem("RandomStackGenerator.dll")> _
    Public Sub LogPrintTest()
        Dim target As Log_Accessor = New Log_Accessor(New Common)
        Dim rnd As New RndValueGen
        Dim log As New List(Of String)

        Dim ok As Boolean = True
        Dim len() As Integer = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000, 10000, 30000}
        Dim time0, time1 As Integer
        Dim t(UBound(len)), txt1, txt2, line As String
        For i As Integer = 0 To UBound(len) Step 1
            log.Clear()
            For k As Integer = 0 To len(i) Step 1
                line = ""
                For m As Integer = 1 To 100 Step 1
                    line &= rnd.RndInt(0, 10000).ToString
                Next m
                log.Add(line)
            Next k
            time0 = Environment.TickCount
            txt1 = target.LogPrint(log)
            time1 = Environment.TickCount
            t(i) = "New: " & time1 - time0
            time0 = Environment.TickCount
            txt2 = LogPrintOld(log, target)
            time1 = Environment.TickCount
            t(i) &= vbTab & "Old: " & time1 - time0

            If Not txt1 = txt2 Then
                ok = False
                Exit For
            End If
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    Private Function LogPrintOld(ByRef log As List(Of String), ByRef target As Log_Accessor) As String
        Dim result As String = ""
        Dim boofer As String = ""
        Dim len As Integer = log.Count - 1
        If len = -1 Then Return result
        result = target.LogPrint(log, 0)
        For i As Integer = 1 To len Step 1
            boofer &= vbNewLine & target.LogPrint(log, i)
            If boofer.Length > 10000 Then
                result &= boofer
                boofer = ""
            End If
        Next i
        If Not boofer = "" Then result &= boofer
        Return result
    End Function

End Class
