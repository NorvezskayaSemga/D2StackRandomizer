Imports System.Collections.Generic

Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator

'''<summary>
'''This is a test class for ObjectsContentSetTest and is intended
'''to contain all ObjectsContentSetTest Unit Tests
'''</summary>
<TestClass()> _
Public Class ObjectsContentSetTest


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

    Dim rndtest As New RandStackTest

    '''<summary>
    '''A test for SetMineType
    '''</summary>
    <TestMethod()> _
    Public Sub SetMineTypeTest()

        If IsNothing(rndtest.UnitsList) Then rndtest.ReadTestUnits()
        If IsNothing(rndtest.ItemsList) Then rndtest.ReadTestItems()
        If IsNothing(rndtest.AllSpells) Then rndtest.ReadTestSpells()

        Dim target As New ObjectsContentSet(rndtest.UnitsList, rndtest.ItemsList, rndtest.excludeList, rndtest.AllSpells)
        Dim ok As Boolean = True
        Dim actual As String

        Dim manaSourcesTypes, generated As New List(Of String)
        manaSourcesTypes.AddRange(New String() {"G000CR0000GR", "G000CR0000RG", "G000CR0000WH", "G000CR0000RD", "G000CR0000YE"})

        For Each s As String In manaSourcesTypes
            actual = target.SetMineType(s)
            If Not actual = s Then ok = False
        Next s
        For i As Integer = 0 To 1000 Step 1
            actual = target.SetMineType(RandomStackGenerator.My.Resources.mineTypeRandomMana)
            If Not manaSourcesTypes.Contains(actual) Then ok = False
            If Not generated.Contains(actual) Then generated.Add(actual)
        Next i
        If Not generated.Count = manaSourcesTypes.Count Then ok = False
        For Each item As String In manaSourcesTypes
            If Not generated.Contains(item) Then ok = False
        Next item

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for MakeSpellsList
    '''</summary>
    <TestMethod()> _
    Public Sub MakeSpellsListTest()

        If IsNothing(rndtest.UnitsList) Then rndtest.ReadTestUnits()
        If IsNothing(rndtest.ItemsList) Then rndtest.ReadTestItems()
        If IsNothing(rndtest.AllSpells) Then rndtest.ReadTestSpells()

        Dim target As New ObjectsContentSet(rndtest.UnitsList, rndtest.ItemsList, rndtest.excludeList, rndtest.AllSpells)
        Dim ok As Boolean = True
        Dim actual As List(Of String)
        Dim input, mines As New List(Of String)
        Dim races() As String = New String() {"H", "U", "E", "C", "L", "R"}
        Dim mass() As String = New String() {"F", "T"}
        Dim c As New AllDataStructues.Cost
        Dim log As New Log(New Common)
        Call log.Enable()

        For i As Integer = 1 To 10 Step 1
            For m1 As Integer = 0 To 1 Step 1
                c.Black = m1
                For m2 As Integer = 0 To 1 Step 1
                    c.Blue = m2
                    For m3 As Integer = 0 To 1 Step 1
                        c.Green = m3
                        For m4 As Integer = 0 To 1 Step 1
                            c.Red = m4
                            For m5 As Integer = 0 To 1 Step 1
                                c.White = m5
                                mines.Clear()
                                If c.Black > 0 Then mines.Add("G000CR0000RG")
                                If c.Blue > 0 Then mines.Add("G000CR0000YE")
                                If c.Green > 0 Then mines.Add("G000CR0000GR")
                                If c.Red > 0 Then mines.Add("G000CR0000RD")
                                If c.White > 0 Then mines.Add("G000CR0000WH")
                                For Each g As String In mass
                                    For Each r As String In races
                                        For level As Integer = 1 To 5 Step 1
                                            If input.Count > 10 Then input.Clear()
                                            input.Add(level & r & g)
                                            actual = target.MakeSpellsList(New AllDataStructues.DesiredStats With {.shopContent = input}, mines, log, -1)
                                            If actual.Count = 0 Then ok = False
                                        Next level
                                    Next r
                                Next g
                            Next
                        Next
                    Next
                Next
            Next
        Next i

        Dim t As Integer = Environment.TickCount
        Dim txt As String = log.PrintAll
        t = Environment.TickCount - t

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for MakeMercenariesList
    '''</summary>
    <TestMethod()> _
    Public Sub MakeMercenariesListTest()

        If IsNothing(rndtest.UnitsList) Then rndtest.ReadTestUnits()
        If IsNothing(rndtest.ItemsList) Then rndtest.ReadTestItems()
        If IsNothing(rndtest.AllSpells) Then rndtest.ReadTestSpells()

        Dim target As New ObjectsContentSet(rndtest.UnitsList, rndtest.ItemsList, rndtest.excludeList, rndtest.AllSpells)
        Dim ok As Boolean = True
        Dim actual As List(Of String)
        Dim input As New List(Of String)
        Dim log As New Log(New Common)
        Call log.Enable()

        For i As Integer = 100 To 10000 Step 100
            'input.Clear()
            input.Add(i)
            actual = target.MakeMercenariesList(New AllDataStructues.DesiredStats With {.shopContent = input}, log, -1)
            If actual.Count = 0 Then ok = False
        Next i

        Dim t As Integer = Environment.TickCount
        Dim txt As String = log.PrintAll
        t = Environment.TickCount - t

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for MakeMerchItemsList
    '''</summary>
    <TestMethod()> _
    Public Sub MakeMerchItemsListTest()

        If IsNothing(rndtest.UnitsList) Then rndtest.ReadTestUnits()
        If IsNothing(rndtest.ItemsList) Then rndtest.ReadTestItems()
        If IsNothing(rndtest.AllSpells) Then rndtest.ReadTestSpells()

        Dim target As New ObjectsContentSet(rndtest.UnitsList, rndtest.ItemsList, rndtest.excludeList, rndtest.AllSpells)
        Dim ok As Boolean = True
        Dim actual As List(Of String)
        Dim input As New List(Of String)
        Dim log As New Log(New Common)
        Call log.Enable()

        For i As Integer = 100 To 10000 Step 100
            'input.Clear()
            input.Add(i)
            actual = target.MakeMerchItemsList(New AllDataStructues.DesiredStats With {.shopContent = input}, log, -1)
            If actual.Count = 0 Then ok = False
        Next i

        Dim t As Integer = Environment.TickCount
        Dim txt As String = log.PrintAll
        t = Environment.TickCount - t

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    ''''<summary>
    ''''A test for LogPrint
    ''''</summary>
    '<TestMethod()> _
    'Public Sub WriteTestSpeed()
    '    Dim textStr, boofer As String
    '    Dim testArray(50000) As String
    '    Dim time(20), bsize As Integer
    '    Dim nStep As Integer = 1000
    '    For i As Integer = 0 To UBound(testArray) Step 1
    '        For j As Integer = 0 To 5 + (i Mod 75) Step 1
    '            testArray(i) &= Chr((i + j) Mod 255)
    '        Next j
    '    Next i
    '
    '    boofer = ""
    '
    '    For b As Integer = 0 To UBound(time) Step 1
    '        bsize = 5000 + nStep * (b + 1)
    '        textStr = ""
    '        time(b) = Environment.TickCount
    '
    '        For i As Integer = 0 To UBound(testArray) Step 1
    '            boofer &= testArray(i)
    '            If boofer.Length > bsize Then
    '                textStr &= boofer
    '                boofer = ""
    '            End If
    '        Next i
    '
    '        time(b) = Environment.TickCount - time(b)
    '
    '    Next b
    '
    '    textStr = ""
    '
    'End Sub

End Class
