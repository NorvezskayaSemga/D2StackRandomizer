Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for WaterGen_WaterPlacer_2Test and is intended
'''to contain all WaterGen_WaterPlacer_2Test Unit Tests
'''</summary>
<TestClass()> _
Public Class WaterGen_WaterPlacer_2Test


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
    '''A test for WaterPlacer_2 WaterBlock Rotation
    '''</summary>
    <TestMethod()> _
    Public Sub WaterGen_WaterPlacer_2_RotationTest()
        Dim grid(,) As Boolean = New Boolean(,) {{True, True, True, False}, _
                                                 {False, False, True, True}, _
                                                 {False, True, False, True}}
        Dim expected_1(,) As Boolean = New Boolean(,) {{False, False, True}, _
                                                       {True, False, True}, _
                                                       {False, True, True}, _
                                                       {True, True, False}}
        Dim expected_2(,) As Boolean = New Boolean(,) {{True, False, True, False}, _
                                                       {True, True, False, False}, _
                                                       {False, True, True, True}}
        Dim expected_3(,) As Boolean = New Boolean(,) {{False, True, True}, _
                                                       {True, True, False}, _
                                                       {True, False, True}, _
                                                       {True, False, False}}
        Dim expected_4(,) As Boolean = grid
        Dim actual(,) As Boolean
        Dim ok As Boolean

        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Rotate90(grid)
        ok = Compare(actual, expected_1)
        If Not ok Then GoTo endtest
        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Rotate90(actual)
        ok = Compare(actual, expected_2)
        If Not ok Then GoTo endtest
        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Rotate90(actual)
        ok = Compare(actual, expected_3)
        If Not ok Then GoTo endtest
        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Rotate90(actual)
        ok = Compare(actual, expected_4)
        If Not ok Then GoTo endtest

endtest:
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    '''<summary>
    '''A test for WaterPlacer_2 WaterBlock Reflection
    '''</summary>
    <TestMethod()> _
    Public Sub WaterGen_WaterPlacer_2_ReflectionTest()
        Dim grid(,) As Boolean = New Boolean(,) {{True, True, True, False}, _
                                                 {False, False, True, True}, _
                                                 {False, True, False, True}}

        Dim expected_H(,) As Boolean = New Boolean(,) {{ False,True, True, True}, _
                                                       {True, True, False, False}, _
                                                       {True, False, True, False}}

        Dim expected_V(,) As Boolean = New Boolean(,) {{False, True, False, True}, _
                                                       {False, False, True, True}, _
                                                       {True, True, True, False}}
        Dim actual(,) As Boolean
        Dim ok As Boolean

        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Reflection(grid, True)
        ok = Compare(actual, expected_H)
        If Not ok Then GoTo endtest
        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Reflection(actual, True)
        ok = Compare(actual, grid)
        If Not ok Then GoTo endtest
        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Reflection(grid, False)
        ok = Compare(actual, expected_V)
        If Not ok Then GoTo endtest
        actual = WaterGen_Accessor.WaterPlacer_2.WaterBlock.Reflection(actual, False)
        ok = Compare(actual, grid)
        If Not ok Then GoTo endtest

endtest:
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    Private Function Compare(ByRef v1(,) As Boolean, ByRef v2(,) As Boolean) As Boolean
        Dim u1 As Integer = UBound(v1, 1)
        Dim u2 As Integer = UBound(v1, 2)
        If Not u1 = UBound(v2, 1) Then Return False
        If Not u2 = UBound(v2, 2) Then Return False
        For i As Integer = 0 To u1 Step 1
            For j As Integer = 0 To u2 Step 1
                If Not v1(i, j) = v2(i, j) Then Return False
            Next j
        Next i
        Return True
    End Function
End Class
