Imports System.Collections.Generic

Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for CommonTest and is intended
'''to contain all CommonTest Unit Tests
'''</summary>
<TestClass()> _
Public Class CommonTest


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
    '''A test for ParseDesiredStackStatsFile
    '''</summary>
    <TestMethod()> _
    Public Sub ParseDesiredStackStatsFileTest1()
        Dim target As Common = New Common()
        Dim ok As Boolean = True
        Dim expected() As AllDataStructues.DesiredStats = New AllDataStructues.DesiredStats() {
            New AllDataStructues.DesiredStats With {.LocationName = "location1", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0}, _
            New AllDataStructues.DesiredStats With {.LocationName = "testloc2", .ExpBarAverage = 100, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 3, .MaxGiants = 1, .MeleeCount = 0, .LootCost = 3400}, _
            New AllDataStructues.DesiredStats With {.LocationName = "loc3", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0}, _
            New AllDataStructues.DesiredStats With {.LocationName = "loc54", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0, _
               .IGen = New AllDataStructues.LootGenSettings With {.ConsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .amount = 1, .costPart = 2}}}, _
            New AllDataStructues.DesiredStats With {.LocationName = "l", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0, _
               .IGen = New AllDataStructues.LootGenSettings With {.ConsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .amount = 1, .costPart = 2}, _
                                                                  .NonconsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .amount = 1, .costPart = 2}}, _
               .isInternalCityGuard = True}, _
            New AllDataStructues.DesiredStats With {.LocationName = "22", .ExpBarAverage = 200, .ExpStackKilled = 75, .MeleeCount = 2, _
                                             .Race = New List(Of Integer), .StackSize = 2, .shopContent = New List(Of String)}, _
            New AllDataStructues.DesiredStats With {.LocationName = "loc53", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0, _
               .IGen = New AllDataStructues.LootGenSettings With {.ConsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .amount = 1, .costPart = 2}, _
                                                                  .JewelItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .amount = 1, .costPart = 2}}}
            }
        expected(0).Race.AddRange(New Integer() {2})
        expected(1).Race.AddRange(New Integer() {2, 9})
        expected(2).Race.AddRange(New Integer() {1})
        expected(3).Race.AddRange(New Integer() {1})
        expected(4).Race.AddRange(New Integer() {1})
        expected(5).Race.AddRange(New Integer() {1})
        expected(6).Race.AddRange(New Integer() {1})

        expected(5).shopContent.AddRange(New String() {"G000UU9999", "1000", "1000", "500"})

        Dim actual() As AllDataStructues.DesiredStats = target.ParseDesiredStackStatsFile(RandomStackGenerator.My.Resources.testFileKeyword)

        If expected.Length = actual.Length Then
            For i As Integer = 0 To UBound(expected) Step 1
                If Not expected(i).ExpBarAverage = actual(i).ExpBarAverage Then ok = False
                If Not expected(i).ExpStackKilled = actual(i).ExpStackKilled Then ok = False
                If Not expected(i).LocationName = actual(i).LocationName Then ok = False
                If Not expected(i).LootCost = actual(i).LootCost Then ok = False
                If Not expected(i).MaxGiants = actual(i).MaxGiants Then ok = False
                If Not expected(i).MeleeCount = actual(i).MeleeCount Then ok = False
                If Not expected(i).StackSize = actual(i).StackSize Then ok = False
                If Not expected(i).IGen.ConsumableItems.exclude = actual(i).IGen.ConsumableItems.exclude Then ok = False
                If Not expected(i).IGen.ConsumableItems.amount = actual(i).IGen.ConsumableItems.amount Then ok = False
                If Not expected(i).IGen.ConsumableItems.costPart = actual(i).IGen.ConsumableItems.costPart Then ok = False
                If Not expected(i).IGen.NonconsumableItems.exclude = actual(i).IGen.NonconsumableItems.exclude Then ok = False
                If Not expected(i).IGen.NonconsumableItems.amount = actual(i).IGen.NonconsumableItems.amount Then ok = False
                If Not expected(i).IGen.NonconsumableItems.costPart = actual(i).IGen.NonconsumableItems.costPart Then ok = False
                If Not expected(i).IGen.JewelItems.exclude = actual(i).IGen.JewelItems.exclude Then ok = False
                If Not expected(i).IGen.JewelItems.amount = actual(i).IGen.JewelItems.amount Then ok = False
                If Not expected(i).IGen.JewelItems.costPart = actual(i).IGen.JewelItems.costPart Then ok = False
                If Not expected(i).isInternalCityGuard = actual(i).isInternalCityGuard Then ok = False
                If expected(i).Race.Count = actual(i).Race.Count Then
                    For Each item As Integer In expected(i).Race
                        If Not actual(i).Race.Contains(item) Then ok = False
                    Next item
                Else
                    ok = False
                End If
                If Not IsNothing(expected(i).shopContent) = IsNothing(actual(i).shopContent) Then
                    ok = False
                ElseIf Not IsNothing(expected(i).shopContent) Then
                    For Each item As String In expected(i).shopContent
                        If Not actual(i).shopContent.Contains(item) Then ok = False
                    Next item
                End If
            Next i
        Else
            ok = False
        End If

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for ParseDesiredStackStatsFile
    '''</summary>
    <TestMethod()> _
    Public Sub ParseDesiredStackStatsFileTest2()
        Dim target As Common = New Common()
        Dim ok As Boolean = True
        Dim path As String = RandomStackGenerator.My.Resources.testFileKeyword
        Dim expected As String = _
        "ID location1 AverageExpBar 1000 ExpStackKilled 200 Race U StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0 IsInternalCityGuard False CItemsGen False#0#0 NItemsGen False#0#0 JItemsGen False#0#0" & vbNewLine & _
        "ID testloc2 AverageExpBar 100 ExpStackKilled 200 Race U+D StackSize 3 MaxGiants 1 MeleeCount 0 LootCost 3400 IsInternalCityGuard False CItemsGen False#0#0 NItemsGen False#0#0 JItemsGen False#0#0" & vbNewLine & _
        "ID loc3 AverageExpBar 1000 ExpStackKilled 200 Race H StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0 IsInternalCityGuard False CItemsGen False#0#0 NItemsGen False#0#0 JItemsGen False#0#0" & vbNewLine & _
        "ID loc54 AverageExpBar 1000 ExpStackKilled 200 Race H StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0 IsInternalCityGuard False CItemsGen True#1#2 NItemsGen False#0#0 JItemsGen False#0#0" & vbNewLine & _
        "ID l AverageExpBar 1000 ExpStackKilled 200 Race H StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0 IsInternalCityGuard True CItemsGen True#1#2 NItemsGen True#1#2 JItemsGen False#0#0" & vbNewLine & _
        "ID 22 ShopContent G000UU9999+1000+1000+500" & vbNewLine & _
        "ID loc53 AverageExpBar 1000 ExpStackKilled 200 Race H StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0 IsInternalCityGuard False CItemsGen True#1#2 NItemsGen False#0#0 JItemsGen True#1#2" & vbNewLine
        Dim content() As AllDataStructues.DesiredStats = target.ParseDesiredStackStatsFile(path)
        Call target.WriteDesiredStackStats(path, content)

        If Not path = expected Then ok = False

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    Dim idArray() As Integer = New Integer() {1, 6, 7, 3, 14, 9, 19}
    Dim someStats() As Double = New Double() {1.9, 2, 1.8, 1.77, 1.87, 1.85, 1.95}
    Dim av As Double = 1.9
    '''<summary>
    '''A test for RandomSelection
    '''</summary>
    <TestMethod()> _
    Public Sub RandomSelectionTest1()
        Dim target As Common = New Common()
        Dim IDs As New List(Of Integer)
        IDs.AddRange(idArray)
        Dim expected(IDs.Max), actual(IDs.Max) As Boolean
        For Each i As Integer In IDs
            expected(i) = True
        Next i
        Dim fullStatsArray(UBound(actual)) As Double
        For i As Integer = 0 To UBound(idArray) Step 1
            fullStatsArray(idArray(i)) = someStats(i)
        Next i
        Dim ok As Boolean = True
        Dim serial As Boolean = False
        For p As Integer = 0 To 1 Step 1
            serial = Not serial
            For i As Integer = 0 To UBound(expected) Step 1
                actual(i) = False
            Next i
            For i As Integer = 0 To 10 * expected.Length Step 1
                actual(target.RandomSelection(IDs, serial)) = True
            Next i
            For i As Integer = 0 To UBound(expected) Step 1
                If Not actual(i) = expected(i) Then ok = False
            Next i
            If Not ok Then Exit For
        Next p
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for RandomSelection
    '''</summary>
    <TestMethod()> _
    Public Sub RandomSelectionTest2()
        Dim target As Common = New Common()
        Dim IDs As New List(Of Integer)
        IDs.AddRange(idArray)
        Dim expected(IDs.Max), actual(IDs.Max) As Boolean
        For Each i As Integer In IDs
            expected(i) = True
        Next i
        Dim fullStatsArray(UBound(actual)) As Double
        For i As Integer = 0 To UBound(idArray) Step 1
            fullStatsArray(idArray(i)) = someStats(i)
        Next i
        Dim ok As Boolean = True
        Dim serial As Boolean = False
        For p As Integer = 0 To 3 Step 1
            If p > 1 Then fullStatsArray = Nothing
            serial = Not serial
            For i As Integer = 0 To UBound(expected) Step 1
                actual(i) = False
            Next i
            For i As Integer = 0 To 10 * expected.Length Step 1
                actual(target.RandomSelection(IDs, fullStatsArray, serial)) = True
            Next i
            For i As Integer = 0 To UBound(expected) Step 1
                If Not actual(i) = expected(i) Then ok = False
            Next i
            If Not ok Then Exit For
        Next p
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for RandomSelection
    '''</summary>
    <TestMethod()> _
    Public Sub RandomSelectionTest3()
        Dim target As Common = New Common()
        Dim IDs As New List(Of Integer)
        IDs.AddRange(idArray)
        Dim expected(IDs.Max), actual(IDs.Max) As Boolean
        For Each i As Integer In IDs
            expected(i) = True
        Next i
        Dim fullStatsArray(UBound(actual)) As Double
        For i As Integer = 0 To UBound(idArray) Step 1
            fullStatsArray(idArray(i)) = someStats(i)
        Next i
        Dim ok As Boolean = True
        Dim serial As Boolean = False
        For p As Integer = 0 To 1 Step 1
            serial = Not serial
            For i As Integer = 0 To UBound(expected) Step 1
                actual(i) = False
            Next i
            For i As Integer = 0 To 10 * expected.Length Step 1
                actual(target.RandomSelection(IDs, {fullStatsArray}, {av}, target.defValues.defaultSigma, serial)) = True
            Next i
            For i As Integer = 0 To UBound(expected) Step 1
                If Not actual(i) = expected(i) Then ok = False
            Next i
            If Not ok Then Exit For
        Next p
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for ReadIntField
    '''</summary>
    <TestMethod(), _
     DeploymentItem("RandomStackGenerator.dll")> _
    Public Sub ReadIntFieldTest()
        Dim target As Common_Accessor = New Common_Accessor()
        Dim ok As Boolean = True
        For i As Integer = 0 To 1000 Step 1
            If Not i = ValueConverter.StrToInt(i.ToString, "", "") Then ok = False
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
