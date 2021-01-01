Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for ValueConverterTest and is intended
'''to contain all ValueConverterTest Unit Tests
'''</summary>
<TestClass()> _
Public Class ValueConverterTest


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
    '''A test for StrToDbl
    '''</summary>
    <TestMethod()> _
    Public Sub StrToDblTest()
        Dim d As String = System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator
        Dim ok As Boolean = True
        Dim expected As Double
        For i As Integer = 0 To 10 Step 1
            For j As Integer = 0 To 1000 Step 1
                expected = CDbl(i.ToString & d & j.ToString)
                If Not expected = ValueConverter.StrToDbl(i.ToString & "." & j.ToString) Then ok = False
                If Not expected = ValueConverter.StrToDbl(i.ToString & "," & j.ToString) Then ok = False
            Next j
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for JoinToLong
    '''</summary>
    <TestMethod()> _
    Public Sub BitJoinTest()
        Dim d As String = "."
        Dim L As Long
        Dim s1, s2, s3 As String
        Dim ok As Boolean = True
        For v1 As Integer = 0 To 1024 Step 1
            s2 = String.Join(d, BitConverter.GetBytes(v1)) & d
            For v2 As Integer = 0 To 1024 Step 1
                L = ValueConverter.BitJoin(v1, v2)
                s1 = String.Join(d, BitConverter.GetBytes(L))
                s3 = s2 & String.Join(d, BitConverter.GetBytes(v2))
                If Not s1 = s3 Then ok = False
            Next v2
        Next v1

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
