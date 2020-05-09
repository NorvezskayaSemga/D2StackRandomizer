Imports System.Threading.Tasks

Public Class RandStack

    Private emptyItem As String = "G000000000"

    Private busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
    Private firstrow() As Integer = New Integer() {0, 2, 4}
    Private secondrow() As Integer = New Integer() {1, 3, 5}
    Private itemGenSigma As Double = 0.5
    Private multiItemGenSigmaMultiplier As Double = 1.5

    Private AllLeaders(), ExcludedUnits() As AllDataStructues.Unit
    Friend AllFighters() As AllDataStructues.Unit
    Friend MagicItem() As AllDataStructues.Item
    Private ExcludedItems() As AllDataStructues.Item
    Public rndgen As RndValueGen
    Public comm As New Common

    Private ExpBarLeaders(), ExpKilledLeaders(), multLeaders() As Double
    Friend ExpBarFighters(), ExpKilledFighters(), multFighters() As Double
    Friend ItemCostSum(), multItems() As Double
    Private minItemGoldCost As Integer

    ''' <summary>Сюда генератор пишет лог</summary>
    Public log As Log

    ''' <param name="AllUnitsList">Dсе юниты в игре</param>
    ''' <param name="AllItemsList">Все предметы в игре</param>
    ''' <param name="ExcludeLists">Файлы со списками исключенных объектов. Записи в них могут повторяться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <summary>Читает множители шанса выпадения для отдельных предметов</summary>
    ''' <param name="LootChanceMultiplierLists">Файлы с множителями шанса появления определенных предметов.
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <param name="CustomUnitRace">Файлы со списками рас юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <param name="SoleUnitsList">Файлы со списками юнитов, которые должны находиться в отряде в единственном экземпляре. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <param name="BigStackUnits">Файлы со списками юнитов, которые должны находиться в отряде начиная с заданного количества слотов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub New(ByRef AllUnitsList() As AllDataStructues.Unit, ByRef AllItemsList() As AllDataStructues.Item, _
                   ByRef ExcludeLists() As String, ByRef LootChanceMultiplierLists() As String, ByRef CustomUnitRace() As String, _
                   ByRef SoleUnitsList() As String, ByRef BigStackUnits() As String)
        rndgen = comm.rndgen
        log = New Log(comm)
        If IsNothing(AllUnitsList) Or IsNothing(AllItemsList) Then Exit Sub

        Call comm.ReadExcludedObjectsList(ExcludeLists)
        Call comm.ReadCustomUnitRace(CustomUnitRace)
        Call comm.ReadLootItemChanceMultiplier(LootChanceMultiplierLists)
        Call comm.ReadSoleUnits(SoleUnitsList)
        Call comm.ReadBigStackUnits(BigStackUnits)

        Dim cat(UBound(AllUnitsList)) As Integer
        For i As Integer = 0 To UBound(AllUnitsList) Step 1
            If Not comm.excludedObjects.Contains(AllUnitsList(i).unitID.ToUpper) Then
                If AllUnitsList(i).unitBranch < 5 Then
                    cat(i) = 1
                ElseIf AllUnitsList(i).unitBranch = 5 Then
                    cat(i) = 2
                Else
                    cat(i) = 0
                End If
            End If
        Next i

        Call MakeAccessoryArrays(AllUnitsList, comm.customRace, AllFighters, cat, 1, ExpBarFighters, ExpKilledFighters, multFighters)
        Call MakeAccessoryArrays(AllUnitsList, comm.customRace, AllLeaders, cat, 2, ExpBarLeaders, ExpKilledLeaders, multLeaders)
        Call MakeAccessoryArrays(AllUnitsList, comm.customRace, ExcludedUnits, cat, 0, Nothing, Nothing, Nothing)

        Call MakeAccessoryArrays(AllItemsList, MagicItem, ExcludedItems, ItemCostSum, multItems)

    End Sub
    Private Sub MakeAccessoryArrays(ByRef allunits() As AllDataStructues.Unit, ByRef customRace As Dictionary(Of String, String), _
                                    ByRef units() As AllDataStructues.Unit, ByRef cat() As Integer, ByRef addcat As Integer, _
                                    ByRef expBar() As Double, ByRef expKuilled() As Double, ByRef mult() As Double)
        Dim n As Integer = -1
        For i As Integer = 0 To UBound(allunits) Step 1
            If cat(i) = addcat Then n += 1
        Next i
        ReDim units(n), expBar(n), expKuilled(n), mult(n)

        n = -1
        For i As Integer = 0 To UBound(allunits) Step 1
            If cat(i) = addcat Then
                n += 1
                units(n) = AllDataStructues.Unit.Copy(allunits(i))
                If customRace.ContainsKey(units(n).unitID) Then
                    units(n).race = comm.RaceIdentifierToSubrace(customRace.Item(units(n).unitID))
                Else
                    units(n).race = comm.RaceIdentifierToSubrace(units(n).race)
                End If
                If Not IsNothing(expBar) Then expBar(n) = units(n).EXPnext
                If Not IsNothing(expKuilled) Then expKuilled(n) = units(n).EXPkilled
                If Not IsNothing(mult) Then
                    If units(n).small Then
                        mult(n) = comm.valConv.smallUnitsExpMultiplicator
                    Else
                        mult(n) = comm.valConv.giantUnitsExpMultiplicator
                    End If
                End If
            End If
        Next i
    End Sub
    Private Sub MakeAccessoryArrays(ByRef allitems() As AllDataStructues.Item, _
                                    ByRef items() As AllDataStructues.Item, _
                                    ByRef excluded() As AllDataStructues.Item, _
                                    ByRef itemCostSum() As Double, ByRef mult() As Double)
        Dim n As Integer = -1
        Dim m As Integer = -1
        Dim add(UBound(allitems)) As Boolean
        For i As Integer = 0 To UBound(allitems) Step 1
            If Not comm.excludedObjects.Contains(allitems(i).itemID.ToUpper) _
            And Not comm.excludedObjects.Contains(comm.itemType.Item(allitems(i).type)) _
            And allitems(i).itemCost.Gold > 0 Then
                add(i) = True
                n += 1
            Else
                m += 1
            End If
        Next i
        ReDim items(n), itemCostSum(n), multItems(n), excluded(m)

        Dim weight As New Dictionary(Of String, String)
        For Each s As String In comm.valConv.WeightMultiplicator.Split(CChar(";"))
            Dim i As Integer = s.IndexOf("=")
            weight.Add(s.Substring(0, i).ToUpper, s.Substring(i + 1).ToUpper)
        Next s

        minItemGoldCost = Integer.MaxValue
        n = -1
        m = -1
        For i As Integer = 0 To UBound(allitems) Step 1
            If add(i) Then
                n += 1
                items(n) = AllDataStructues.Item.Copy(allitems(i))
                itemCostSum(n) = AllDataStructues.Cost.Sum(LootCost(items(n)))
                mult(n) = ItemTypeWeight(weight, comm.itemType.Item(items(n).type), itemCostSum(n))
                If comm.LootItemChanceMultiplier.ContainsKey(items(n).itemID.ToUpper) Then
                    mult(n) *= comm.LootItemChanceMultiplier.Item(items(n).itemID.ToUpper)
                End If
                minItemGoldCost = Math.Min(minItemGoldCost, CInt(items(n).itemCost.Gold))
            Else
                m += 1
                excluded(m) = AllDataStructues.Item.Copy(allitems(i))
            End If
        Next i
    End Sub
    Private Function ItemTypeWeight(ByRef wList As Dictionary(Of String, String), ByRef itemType As String, ByRef cost As Double) As Double
        If Not wList.ContainsKey(itemType) Then Return 1
        Dim s() As String = wList.Item(itemType).Split(CChar("#"))
        Dim w1 As Double = CDbl(s(0))
        If s.Length > 1 Then
            Dim d2 As String = "ELSE"
            If s(1).Contains(">") Then
                Dim d1 As Char = CChar(">")
                Dim v As Double = CDbl(s(1).Split(d1)(1).Split(New String() {d2}, StringSplitOptions.RemoveEmptyEntries)(0))
                Dim w2 As Double = CDbl(s(1).Split(d1)(1).Split(New String() {d2}, StringSplitOptions.RemoveEmptyEntries)(1))
                If cost > v Then
                    Return w1
                Else
                    Return w2
                End If
            ElseIf s(1).Contains("<") Then
                Dim d1 As Char = CChar("<")
                Dim v As Double = CDbl(s(1).Split(d1)(1).Split(New String() {d2}, StringSplitOptions.RemoveEmptyEntries)(0))
                Dim w2 As Double = CDbl(s(1).Split(d1)(1).Split(New String() {d2}, StringSplitOptions.RemoveEmptyEntries)(1))
                If cost < v Then
                    Return w1
                Else
                    Return w2
                End If
            ElseIf s(1).Contains("=") Then
                Dim d1 As Char = CChar("=")
                Dim v As Double = CDbl(s(1).Split(d1)(1).Split(New String() {d2}, StringSplitOptions.RemoveEmptyEntries)(0))
                Dim w2 As Double = CDbl(s(1).Split(d1)(1).Split(New String() {d2}, StringSplitOptions.RemoveEmptyEntries)(1))
                If cost = v Then
                    Return w1
                Else
                    Return w2
                End If
            Else
                Throw New Exception("Invalid compare action")
            End If
        End If
        Return w1
    End Function

    ''' <summary>Найдет статы юнита по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxUUxxxx</param>
    Public Function FindUnitStats(ByVal ID As String) As AllDataStructues.Unit
        Dim f As String = ID.ToUpper
        Dim a()() As AllDataStructues.Unit = New AllDataStructues.Unit()() {AllFighters, AllLeaders, ExcludedUnits}
        For u As Integer = 0 To UBound(a) Step 1
            If Not IsNothing(a(u)) Then
                For i As Integer = 0 To UBound(a(u)) Step 1
                    If f = a(u)(i).unitID Then Return AllDataStructues.Unit.Copy(a(u)(i))
                Next i
            End If
        Next u
        Return Nothing
    End Function
    ''' <summary>Найдет статы предмета по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxIGxxxx</param>
    Public Function FindItemStats(ByVal ID As String) As AllDataStructues.Item
        Dim f As String = ID.ToUpper
        Dim a()() As AllDataStructues.Item = New AllDataStructues.Item()() {MagicItem, ExcludedItems}
        For u As Integer = 0 To UBound(a) Step 1
            If Not IsNothing(a(u)) Then
                For i As Integer = 0 To UBound(a(u)) Step 1
                    If f = a(u)(i).itemID Then Return AllDataStructues.Item.Copy(a(u)(i))
                Next i
            End If
        Next u
        Return Nothing
    End Function

    ''' <summary>Вычисляет параметры отряда по составу. Цена предмета в мане прибавится к стоимости лута в золоте</summary>
    ''' <param name="stack">ID юнитов и предметов отряда</param>
    Public Function StackStats(ByRef stack As AllDataStructues.Stack) As AllDataStructues.DesiredStats
        Dim result As New AllDataStructues.DesiredStats With {.Race = New List(Of Integer)}
        Dim unit As AllDataStructues.Unit
        For i As Integer = 0 To UBound(stack.pos) Step 1
            If Not stack.pos(i).ToUpper = emptyItem Then
                unit = FindUnitStats(stack.pos(i))
                If unit.unitID = "" Then Throw New Exception("Неизвестный id юнита: " & stack.pos(i))
                If Not result.Race.Contains(unit.race) Then result.Race.Add(unit.race)
                result.ExpStackKilled += unit.EXPkilled
                result.ExpBarAverage += unit.EXPnext
                If unit.small Then
                    result.StackSize += 1
                Else
                    result.StackSize += 2
                    result.MaxGiants += 1
                End If
                If Not unit.small Or unit.reach = 3 Then result.MeleeCount += 1
                If unit.level < stack.level(i) Then
                    Dim d As Integer = stack.level(i) - unit.level
                    If d < unit.dynUpgradeLevel Then
                        result.ExpStackKilled += unit.dynUpgrade1.EXPkilled * d
                        result.ExpBarAverage += unit.dynUpgrade1.EXPnext * d
                    Else
                        result.ExpStackKilled += unit.dynUpgrade1.EXPkilled * (unit.dynUpgradeLevel - 1)
                        result.ExpBarAverage += unit.dynUpgrade1.EXPnext * (unit.dynUpgradeLevel - 1)
                        d -= unit.dynUpgradeLevel - 1
                        result.ExpStackKilled += unit.dynUpgrade2.EXPkilled * d
                        result.ExpBarAverage += unit.dynUpgrade2.EXPnext * d
                    End If
                End If
            End If
        Next i
        If result.StackSize > 0 Then result.ExpBarAverage = CInt(result.ExpBarAverage / result.StackSize)

        Dim LCost As AllDataStructues.Cost = LootCost(stack.items)
        result.LootCost = AllDataStructues.Cost.Sum(LCost)
        result.IGen = GetItemsGenSettings(stack.items)

        Return result
    End Function
    ''' <summary>Определит настройки генерации новых предметов</summary>
    ''' <param name="items">Список предметов объекта</param>
    Public Function GetItemsGenSettings(ByRef items As List(Of String)) As AllDataStructues.LootGenSettings
        If IsNothing(items) Then Return New AllDataStructues.LootGenSettings
        Dim result As New AllDataStructues.LootGenSettings With { _
            .ConsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True}, _
            .NonconsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True}, _
            .JewelItems = New AllDataStructues.ItemGenSettings With {.exclude = True}}
        Dim item As AllDataStructues.Item
        For Each id As String In items
            item = FindItemStats(id)
            If comm.ConsumableItemsTypes.Contains(item.type) Then
                result.ConsumableItems.exclude = False
                result.ConsumableItems.amount += 1
                result.ConsumableItems.costPart += AllDataStructues.Cost.Sum(LootCost(item))
            ElseIf comm.NonconsumableItemsTypes.Contains(item.type) Then
                result.NonconsumableItems.exclude = False
                result.NonconsumableItems.amount += 1
                result.NonconsumableItems.costPart += AllDataStructues.Cost.Sum(LootCost(item))
            ElseIf comm.JewelItemsTypes.Contains(item.type) Then
                result.JewelItems.exclude = False
                result.JewelItems.amount += 1
                result.JewelItems.costPart += AllDataStructues.Cost.Sum(LootCost(item))
            End If
        Next id
        Dim sum As Double = result.ConsumableItems.costPart + result.NonconsumableItems.costPart + result.JewelItems.costPart
        If sum > 0 Then
            sum = 1 / sum
            result.ConsumableItems.costPart *= sum
            result.NonconsumableItems.costPart *= sum
            result.JewelItems.costPart *= sum
        End If
        Return result
    End Function

    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items As List(Of AllDataStructues.Item)) As AllDataStructues.Cost
        Dim result As AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As AllDataStructues.Item In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items() As AllDataStructues.Item) As AllDataStructues.Cost
        Dim result As AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As AllDataStructues.Item In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items As List(Of String)) As AllDataStructues.Cost
        Dim result As AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As String In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items() As String) As AllDataStructues.Cost
        Dim result As AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As String In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет ценность предмета</summary>
    ''' <param name="item">Предмет</param>
    Public Function LootCost(ByRef item As AllDataStructues.Item) As AllDataStructues.Cost
        Return comm.ItemTypeCostModify(item)
    End Function
    ''' <summary>Определяет ценность предмета</summary>
    ''' <param name="item">Предмет</param>
    Public Function LootCost(ByVal item As String) As AllDataStructues.Cost
        Dim m As AllDataStructues.Item = FindItemStats(item)
        If m.itemID = "" Then Throw New Exception("Неизвестный id предмета: " & item)
        Return LootCost(m)
    End Function

    ''' <summary>Может быть преобразует часть золота в ману. Результат будет кратен 50</summary>
    ''' <param name="input">Начальные ресурсы. При конвертации начальная мана не пропадет</param>
    ''' <param name="conversionChance">Шанс сконвертировать (от 0 до 1)</param>
    ''' <param name="conversionAmount">Какую часть золота сконвертировать (от 0 до 1)</param>
    ''' <param name="outputMana">1 - black, 2 - blue, 3 - green, 4 - red, 5 - white</param>
    Public Function GoldToMana(ByRef input As AllDataStructues.Cost, ByVal conversionChance As Double, _
                               ByVal conversionAmount As Double, ByRef outputMana As List(Of Integer)) As AllDataStructues.Cost
        Dim output As AllDataStructues.Cost = AllDataStructues.Cost.Copy(input)
        If conversionChance > 0 AndAlso outputMana.Count > 0 AndAlso rndgen.Rand(0, 1, True) <= conversionChance Then
            Dim dAmount As Integer = 50
            Dim amount As Integer = 0
            Do While (amount + dAmount) * outputMana.Count <= Math.Floor(input.Gold * Math.Min(conversionAmount, 1))
                amount += dAmount
            Loop
            output.Gold -= amount * outputMana.Count
            For Each manaID As Integer In outputMana
                If manaID = 1 Then
                    output.Black += amount
                ElseIf manaID = 2 Then
                    output.Blue += amount
                ElseIf manaID = 3 Then
                    output.Green += amount
                ElseIf manaID = 4 Then
                    output.Red += amount
                ElseIf manaID = 5 Then
                    output.White += amount
                Else
                    Throw New Exception("Unknown mana ID: " & manaID)
                End If
            Next manaID
        End If
        Return output
    End Function

    Private Sub AddToLog(ByRef LogID As Integer, ByRef Msg As String)
        If LogID > -1 Then
            Call log.MAdd(LogID, Msg)
        Else
            Call log.Add(Msg)
        End If
    End Sub
    Private Sub AddToLog(ByRef LogID As Integer, ByRef IGen As AllDataStructues.LootGenSettings)
        If LogID > -1 Then
            Call log.MAdd(LogID, IGen)
        Else
            Call log.Add(IGen)
        End If
    End Sub
    Private Sub AddToLog(ByRef LogID As Integer, ByRef DesiredStats As AllDataStructues.DesiredStats, ByRef shortOut As Boolean)
        If LogID > -1 Then
            Call log.MAdd(LogID, DesiredStats, shortOut)
        Else
            Call log.Add(DesiredStats, shortOut)
        End If
    End Sub
    Public Sub AddToLog(ByVal LogID As Integer, ByRef contString As Log.printSelectionList, ByVal v() As AllDataStructues.Unit, ByRef i As List(Of Integer))
        If LogID > -1 Then
            Call log.MAdd(LogID, contString, v, i)
        Else
            Call log.Add(contString, v, i)
        End If
    End Sub

    ''' <summary>Генерирует набор предметов. В принципе может вернуть пустой список</summary>
    ''' <param name="GoldCost">Максимальная стоимость набора в золоте. Драгоценности считаются дешевле в два раза</param>
    ''' <param name="IGen">Настройки генерации предметов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ItemsGen(ByVal GoldCost As Integer, _
                             ByVal IGen As AllDataStructues.LootGenSettings, _
                             Optional ByVal LogID As Integer = -1) As List(Of String)
        Call AddToLog(LogID, "----Loot creation started----" & vbNewLine & _
                             "Gold sum: " & GoldCost)
        Call AddToLog(LogID, IGen)

        Dim DynIGen As AllDataStructues.LootGenSettings = GenItemSetDynIGen(IGen, GoldCost)
        Dim serialExecution As Boolean = (LogID < 0)
        Dim costBar(), maxCost(), selected As Integer
        Dim weight(UBound(MagicItem)) As Double
        Dim DynCost As Integer = GoldCost
        Dim IDs As New List(Of Integer)
        Dim result As New List(Of String)
        Do While DynCost >= minItemGoldCost
            maxCost = GenItemMaxCost(DynIGen, DynCost)
            costBar = GenItemCostBar(DynIGen, maxCost, serialExecution)
            Call AddToLog(LogID, "Max cost bar:" & DynCost & _
                                 " Selected cost bar:" & costBar(0) & "|" & costBar(1) & "|" & costBar(2) & _
                                 " max item cost:" & maxCost(0) & "|" & maxCost(1) & "|" & maxCost(2))
            IDs.Clear()
            For i As Integer = 0 To UBound(MagicItem) Step 1
                If ItemFilter(DynIGen, MagicItem(i), costBar) Then
                    IDs.Add(i)
                    weight(i) = GenItemWeight(MagicItem(i), costBar) * multItems(i)
                Else
                    weight(i) = 0
                End If
            Next i
            If IDs.Count = 0 Then Exit Do

            selected = comm.RandomSelection(IDs, weight, serialExecution)

            Call AddToLog(LogID, "Selected item:" & MagicItem(selected).name & " id:" & MagicItem(selected).itemID & " cost:" & ItemCostSum(selected))
            result.Add(MagicItem(selected).itemID)
            Call GenItemIGenChange(DynIGen, MagicItem(selected), DynCost)
            DynCost = CInt(DynCost - ItemCostSum(selected))
        Loop

        Call AddToLog(LogID, "----Loot creation ended----")

        Return result
    End Function
    ''' <summary>Генерирует набор предметов. В принципе может вернуть пустой список</summary>
    ''' <param name="GoldCost">Максимальная стоимость набора в золоте. Драгоценности считаются дешевле в два раза</param>
    ''' <param name="IGen">Настройки генерации предметов</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ItemsGen(ByVal GoldCost As Integer, _
                             ByRef IGen As AllDataStructues.LootGenSettings, _
                             ByVal LootCostMultiplier As Double, _
                             Optional ByVal LogID As Integer = -1) As List(Of String)
        Return ItemsGen(CInt(GoldCost * LootCostMultiplier), IGen, LogID)
    End Function
    Private Function CostBarGen(ByRef minBar As Integer, ByRef maxBar As Integer, ByRef serialExecution As Boolean) As Integer
        'Return CInt(rndgen.Rand(CDbl(minBar), CDbl(maxBar), serialExecution))
        Dim R As Double = rndgen.Rand(0, 1, serialExecution)
        Dim G As Double = 3
        Dim D As Double = 0.15
        Dim S As Double = 5
        Dim E As Double = 1 / (1 + Math.Exp(S * D))
        Dim V As Double = (1 - G * E) / (1 - E)
        Dim m As Double = V + (G - V) / (1 + Math.Exp(S * (D - R)))
        Dim bar As Double = 1 - (1 - R) / m
        Return minBar + CInt(bar * CDbl(maxBar - minBar))
    End Function
    ''' <summary>Генерирует один предмет. Если не получится выбрать подходящий предмет, вернет пустую строку</summary>
    ''' <param name="GoldCost">Максимальная стоимость предмета в золоте. Драгоценности считаются дешевле в два раза</param>
    ''' <param name="IGen">Настройки генерации предметов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ThingGen(ByVal GoldCost As Integer, _
                             ByRef IGen As AllDataStructues.LootGenSettings, _
                             Optional ByVal LogID As Integer = -1) As String

        Call AddToLog(LogID, "----Single item creation started----" & vbNewLine & _
                            "Max cost: " & GoldCost)
        Call AddToLog(LogID, IGen)

        Dim serialExecution As Boolean = (LogID < 0)
        Dim selected As Integer
        Dim IDs As New List(Of Integer)
        Dim result As String = ""

        IDs.Clear()
        For i As Integer = 0 To UBound(MagicItem) Step 1
            If ItemCostSum(i) <= GoldCost AndAlso ItemFilter(IGen, MagicItem(i)) Then IDs.Add(i)
        Next i
        If IDs.Count > 0 Then
            selected = comm.RandomSelection(IDs, New Double()() {ItemCostSum}, New Double() {GoldCost}, multItems, itemGenSigma, serialExecution)
            Call AddToLog(LogID, "Selected item:" & MagicItem(selected).name & " id:" & MagicItem(selected).itemID & " cost:" & ItemCostSum(selected))
            result = MagicItem(selected).itemID
        End If

        Call AddToLog(LogID, "----Single item creation ended----")

        Return result
    End Function
    ''' <summary>Генерирует один предмет. Если не получится выбрать подходящий предмет, вернет пустую строку</summary>
    ''' <param name="GoldCost">Максимальная стоимость предмета в золоте. Драгоценности считаются дешевле в два раза</param>
    ''' <param name="IGen">Настройки генерации предметов</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ThingGen(ByVal GoldCost As Integer, _
                             ByRef IGen As AllDataStructues.LootGenSettings, _
                             ByVal LootCostMultiplier As Double, _
                             Optional ByVal LogID As Integer = -1) As String
        Return ThingGen(CInt(GoldCost * LootCostMultiplier), IGen, LogID)
    End Function
    Private Function GenItemSetDynIGen(ByRef IGen As AllDataStructues.LootGenSettings, ByRef GoldCost As Integer) As AllDataStructues.LootGenSettings
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        Dim weightsSum As Double
        For i As Integer = 0 To UBound(settings) Step 1
            If Not settings(i).exclude Then weightsSum += settings(i).costPart
        Next i
        If weightsSum > 1 Then Throw New Exception("Invalid cost parts sum: " & weightsSum)
        Dim Dyn As AllDataStructues.LootGenSettings = AllDataStructues.LootGenSettings.Copy(IGen)
        Dyn.ConsumableItems.dynCostPart = CInt(GoldCost * Dyn.ConsumableItems.costPart)
        Dyn.NonconsumableItems.dynCostPart = CInt(GoldCost * Dyn.NonconsumableItems.costPart)
        Dyn.JewelItems.dynCostPart = CInt(GoldCost * Dyn.JewelItems.costPart)
        Return Dyn
    End Function
    Private Function GenItemMaxCost(ByRef IGen As AllDataStructues.LootGenSettings, ByRef GoldCost As Integer) As Integer()
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        Dim result(UBound(settings)) As Integer
        For i As Integer = 0 To UBound(settings) Step 1
            If Not settings(i).exclude Then
                If settings(i).costPart > 0 Then
                    result(i) = settings(i).dynCostPart
                Else
                    result(i) = GoldCost
                End If
            End If
        Next i
        Return result
    End Function
    Private Function GenItemCostBar(ByRef IGen As AllDataStructues.LootGenSettings, ByRef MaxCost() As Integer, _
                                    ByRef serialExecution As Boolean) As Integer()
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        Dim result(UBound(settings)), min, max As Integer
        Dim upCost As Integer = CInt(0.75 * minItemGoldCost)
        For i As Integer = 0 To UBound(settings) Step 1
            If Not settings(i).exclude Then
                If settings(i).amount > 0 And settings(i).dynCostPart > 0 Then
                    max = CInt(Math.Max(minItemGoldCost, (settings(i).dynCostPart + upCost) / settings(i).amount))
                    min = minItemGoldCost + CInt(0.75 * (max - minItemGoldCost))
                ElseIf settings(i).amount > 0 Then
                    max = CInt(Math.Max(minItemGoldCost, (MaxCost(i) + upCost) / settings(i).amount))
                    min = minItemGoldCost + CInt(0.75 * (max - minItemGoldCost))
                Else
                    max = Math.Max(minItemGoldCost, MaxCost(i) + upCost)
                    min = minItemGoldCost
                End If
                result(i) = CostBarGen(min, max, serialExecution)
            End If
        Next i
        Return result
    End Function
    Private Function GenItemWeight(ByRef item As AllDataStructues.Item, ByRef CostBar() As Integer) As Double
        Dim result As Double
        For i As Integer = 0 To UBound(CostBar) Step 1
            If comm.ItemTypesLists(i).Contains(item.type) Then
                result = comm.Gauss(AllDataStructues.Cost.Sum(LootCost(item)), _
                                    CostBar(i), multiItemGenSigmaMultiplier * itemGenSigma)
                Exit For
            End If
        Next i
        Return result
    End Function
    Private Sub GenItemIGenChange(ByRef IGen As AllDataStructues.LootGenSettings, _
                                  ByRef item As AllDataStructues.Item, _
                                  ByRef DynCost As Integer)
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        For i As Integer = 0 To UBound(settings) Step 1
            If comm.ItemTypesLists(i).Contains(item.type) Then
                If settings(i).amount > 0 Then
                    settings(i).amount -= 1
                    If settings(i).amount = 0 Then
                        Dim weightsSum As Double
                        For j As Integer = 0 To UBound(settings) Step 1
                            If Not settings(j).exclude Then weightsSum += settings(j).costPart
                        Next j
                        If weightsSum > 0 Then
                            settings(i).costPart = 1 - weightsSum
                            settings(i).dynCostPart = CInt(settings(i).costPart * DynCost)
                        End If
                    End If
                End If
                If settings(i).dynCostPart > 0 Then
                    settings(i).dynCostPart -= AllDataStructues.Cost.Sum(LootCost(item))
                    If settings(i).dynCostPart <= 0 Then
                        settings(i).dynCostPart = 0
                        For j As Integer = 0 To UBound(settings) Step 1
                            If Not i = j And Not settings(j).exclude Then settings(i).exclude = True
                        Next j
                    End If
                End If
                If i = 0 Then
                    IGen.ConsumableItems = settings(i)
                ElseIf i = 1 Then
                    IGen.NonconsumableItems = settings(i)
                ElseIf i = 2 Then
                    IGen.JewelItems = settings(i)
                Else
                    Throw New Exception
                End If
                Exit For
            End If
        Next i
    End Sub

    Friend Function ItemFilter(ByRef IGen As AllDataStructues.LootGenSettings, ByRef item As AllDataStructues.Item) As Boolean
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        For i As Integer = 0 To UBound(settings) Step 1
            If comm.ItemTypesLists(i).Contains(item.type) Then
                If settings(i).exclude Then
                    Return False
                Else
                    Exit For
                End If
            End If
        Next i
        Return True
    End Function
    Friend Function ItemFilter(ByRef IGen As AllDataStructues.LootGenSettings, ByRef item As AllDataStructues.Item, _
                               ByRef CostBar() As Integer) As Boolean
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        For i As Integer = 0 To UBound(settings) Step 1
            If comm.ItemTypesLists(i).Contains(item.type) Then
                If settings(i).exclude Then
                    Return False
                Else
                    If AllDataStructues.Cost.Sum(LootCost(item)) > 2 * CostBar(i) Then Return False
                    Exit For
                End If
            End If
        Next i
        Return True
    End Function

    ''' <summary>Затычка: вернет отряд из двух сквайров и трех лучников. Лидер - паладин. С зельем воскрешения</summary>
    Public Function GenGag() As AllDataStructues.Stack
        Dim result As New AllDataStructues.Stack
        ReDim result.pos(UBound(busytransfer)), result.level(UBound(busytransfer))
        Dim fighter1 As String = "G000UU0001"
        Dim fighter2 As String = "G000UU0006"
        Dim leader As String = "G000UU5356"
        For i As Integer = 0 To UBound(firstrow) Step 1
            result.pos(firstrow(i)) = fighter1
            result.level(firstrow(i)) = 1
        Next i
        For i As Integer = 0 To UBound(secondrow) Step 1
            result.pos(secondrow(i)) = fighter2
            result.level(secondrow(i)) = 1
        Next i
        result.pos(firstrow(2)) = leader
        result.leaderPos = firstrow(2)
        result.items = New List(Of String)
        result.items.Add("G000IG0001")
        Return result
    End Function

    ''' <summary>Создаст отряд  в соответствие с желаемыми параметрами. Не нужно пытаться создать отряд водных жителей на земле</summary>
    ''' <param name="StackStats">Желаемые параметры стэка</param>
    ''' <param name="deltaLeadership">Изменение лидерства за счет модификаторов</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров. Водной считается клетка с водой, окруженная со всех сторон клетками с водой</param>
    ''' <param name="NoLeader">True, если стэк находится внутри руин или города</param>
    Public Function Gen(ByRef StackStats As AllDataStructues.DesiredStats, _
                        ByVal deltaLeadership As Integer, ByVal GroundTile As Boolean, _
                        ByVal NoLeader As Boolean) As AllDataStructues.Stack

        If Not IsNothing(StackStats.shopContent) Then Return Nothing

        Dim DynStackStats As AllDataStructues.DesiredStats = AllDataStructues.DesiredStats.Copy(StackStats)
        DynStackStats.Race.Clear()
        For Each i As Integer In StackStats.Race
            Dim s As Integer = comm.RaceIdentifierToSubrace(i)
            If Not DynStackStats.Race.Contains(s) Then DynStackStats.Race.Add(s)
        Next i

        Call log.Add(vbNewLine & "----Stack creation started----")
        Call log.Add("DeltaLeadership: " & deltaLeadership & " GroundTile: " & GroundTile & " NoLeader: " & NoLeader)
        Call log.Add(DynStackStats, False)

        Dim result As AllDataStructues.Stack = GenStackMultithread(StackStats, DynStackStats, deltaLeadership, GroundTile, NoLeader)

        result.items = ItemsGen(DynStackStats.LootCost, DynStackStats.IGen)

        Call log.Add("----Stack creation ended----")

        Return result
    End Function
    ''' <summary>Создаст отряд  в соответствие с желаемыми параметрами. Не нужно пытаться создать отряд водных жителей на земле</summary>
    ''' <param name="ExpStackKilled">Примерный опыт за убийство стэка</param>
    ''' <param name="LootCost">Стоимость лута (предметы со стоимостью в золоте, равной нулю, не добавляются). При расчете стоимость драгоценностей уменьшается в два раза</param>
    ''' <param name="Races">Допустимые расы юнитов в отряде</param>
    ''' <param name="IGen">Настройки генерации предметов</param>
    ''' <param name="deltaLeadership">Изменение лидерства за счет модификаторов</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров. Водной считается клетка с водой, окруженная со всех сторон клетками с водой</param>
    ''' <param name="NoLeader">True, если стэк находится внутри руин или города</param>
    Public Function Gen(ByVal ExpStackKilled As Integer, ByVal LootCost As Double, ByRef Races As List(Of Integer), _
                        ByRef IGen As AllDataStructues.LootGenSettings, ByVal deltaLeadership As Integer, _
                        ByVal GroundTile As Boolean, ByVal NoLeader As Boolean) As AllDataStructues.Stack
        Dim StackStat As AllDataStructues.DesiredStats = StackStatsGen.GenDesiredStats(CDbl(ExpStackKilled), _
                                                                                       LootCost, rndgen, comm.valConv)
        StackStat.Race = Races
        StackStat.IGen = IGen
        Return Gen(StackStat, deltaLeadership, GroundTile, NoLeader)
    End Function
    ''' <summary>Создаст отряд  в соответствие с желаемыми параметрами. Не нужно пытаться создать отряд водных жителей на земле</summary>
    ''' <param name="StackStats">Желаемые параметры стэка</param>
    ''' <param name="deltaLeadership">Изменение лидерства за счет модификаторов</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров. Водной считается клетка с водой, окруженная со всех сторон клетками с водой</param>
    ''' <param name="NoLeader">True, если стэк находится внутри руин или города</param>
    ''' <param name="StackStrengthMultiplier">Множитель силы отряда: изменяем опыт за убийство и среднюю планку опыта</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    Public Function Gen(ByRef StackStats As AllDataStructues.DesiredStats, ByVal deltaLeadership As Integer, _
                        ByVal GroundTile As Boolean, ByVal NoLeader As Boolean, _
                        ByVal StackStrengthMultiplier As Double, ByVal LootCostMultiplier As Double) As AllDataStructues.Stack
        Dim s As AllDataStructues.DesiredStats = AllDataStructues.DesiredStats.Copy(StackStats)
        s.ExpStackKilled = Math.Max(CInt(s.ExpStackKilled * StackStrengthMultiplier), 5)
        s.ExpBarAverage = Math.Max(CInt(s.ExpBarAverage * StackStrengthMultiplier), 25)
        s.LootCost = CInt(s.LootCost * LootCostMultiplier)
        Return Gen(s, deltaLeadership, GroundTile, NoLeader)
    End Function
    ''' <summary>Создаст отряд  в соответствие с желаемыми параметрами. Не нужно пытаться создать отряд водных жителей на земле</summary>
    ''' <param name="ExpStackKilled">Примерный опыт за убийство стэка</param>
    ''' <param name="LootCost">Стоимость лута (предметы со стоимостью в золоте, равной нулю, не добавляются). При расчете стоимость драгоценностей уменьшается в два раза</param>
    ''' <param name="Races">Допустимые расы юнитов в отряде</param>
    ''' <param name="IGen">Настройки генерации предметов</param>
    ''' <param name="deltaLeadership">Изменение лидерства за счет модификаторов</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров. Водной считается клетка с водой, окруженная со всех сторон клетками с водой</param>
    ''' <param name="NoLeader">True, если стэк находится внутри руин или города</param>
    ''' <param name="StackStrengthMultiplier">Множитель силы отряда: изменяем опыт за убийство и среднюю планку опыта</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    Public Function Gen(ByVal ExpStackKilled As Integer, ByVal LootCost As Double, ByRef Races As List(Of Integer), _
                        ByRef IGen As AllDataStructues.LootGenSettings, ByVal deltaLeadership As Integer, _
                        ByVal GroundTile As Boolean, ByVal NoLeader As Boolean, _
                        ByVal StackStrengthMultiplier As Double, ByVal LootCostMultiplier As Double) As AllDataStructues.Stack
        Dim esk As Integer = Math.Max(CInt(ExpStackKilled * StackStrengthMultiplier), 5)
        Dim lc As Double = LootCost * LootCostMultiplier
        Return Gen(esk, lc, Races, IGen, deltaLeadership, GroundTile, NoLeader)
    End Function
    Private Function SelectPossibleLeader(ByRef leaderID As Integer, ByRef Tolerance As Double, _
                                          ByRef StackStats As AllDataStructues.DesiredStats, ByRef GroundTile As Boolean) As Boolean
        If Not StackStats.Race.Contains(AllLeaders(leaderID).race) Then Return False
        If Not AllLeaders(leaderID).small And StackStats.MaxGiants = 0 Then Return False
        If AllLeaders(leaderID).waterOnly And GroundTile Then Return False

        If comm.BigStackUnits.ContainsKey(AllLeaders(leaderID).unitID) _
        AndAlso StackStats.StackSize < comm.BigStackUnits.Item(AllLeaders(leaderID).unitID) Then Return False

        'Dim mult As Double
        'If AllLeaders(leaderID).small Then
        '    mult = 1
        'Else
        '    mult = 2
        'End If
        'If Math.Abs(AllLeaders(leaderID).EXPnext - mult * StackStats.ExpBarAverage) _
        '    > mult * Tolerance * StackStats.ExpBarAverage Then Return False
        If AllLeaders(leaderID).EXPkilled > (1 + Tolerance) * StackStats.ExpStackKilled Then Return False
        Return True
    End Function
    Private Function SigmaMultiplier(ByRef stat As AllDataStructues.DesiredStats) As Double
        Return comm.valConv.defaultSigma * (CDbl(stat.StackSize) + 1.25 * CDbl(stat.StackSize * stat.StackSize - 1) + 0.2 * CDbl(stat.MaxGiants))
    End Function

    Private Function GenStackMultithread(ByVal StackStats As AllDataStructues.DesiredStats, _
                                         ByVal BakDynStackStats As AllDataStructues.DesiredStats, _
                                         ByVal deltaLeadership As Integer, ByVal GroundTile As Boolean, _
                                         ByVal NoLeader As Boolean) As AllDataStructues.Stack

        Dim units(11)() As AllDataStructues.Unit
        Dim DynStackStats(UBound(units)) As AllDataStructues.DesiredStats
        Call log.MRedim(units.Length)

        Parallel.For(0, units.Length, _
         Sub(jobID As Integer)
             log.MAdd(jobID, "--------Attempt " & jobID + 1 & " started--------")
             Dim FreeMeleeSlots As Integer = 3
             Dim BaseStackSize As Integer
             DynStackStats(jobID) = AllDataStructues.DesiredStats.Copy(BakDynStackStats)
             Dim SelectedLeader As Integer = GenLeader(StackStats, DynStackStats(jobID), FreeMeleeSlots, deltaLeadership, _
                                                       GroundTile, NoLeader, CDbl(jobID / units.Length), jobID)
             BaseStackSize = DynStackStats(jobID).StackSize
             If Not NoLeader Then
                 If AllLeaders(SelectedLeader).small Then
                     BaseStackSize += 1
                 Else
                     BaseStackSize += 2
                 End If
             End If

             Dim SelectedFighters As List(Of Integer) = GenFingters(StackStats, DynStackStats(jobID), FreeMeleeSlots, _
                                                                    SelectedLeader, GroundTile, BaseStackSize, _
                                                                    CDbl(jobID / units.Length), jobID)
             units(jobID) = GenUnitsList(SelectedFighters, SelectedLeader, NoLeader)
             log.MAdd(jobID, "--------Attempt " & jobID + 1 & " ended--------")
         End Sub)

        Call log.Add(log.MPrintAll())
        Call log.MRedim(0)

        Dim selected As Integer = SelectStack(StackStats, DynStackStats)

        If log.IsEnabled Then
            Dim txt As String = ""
            For Each unit As AllDataStructues.Unit In units(selected)
                txt &= vbNewLine & unit.unitID & " " & unit.name
            Next unit
            Call log.Add("--------Selected Stack--------" & txt)
        End If

        Return GenPositions(StackStats, DynStackStats(selected), units(selected))
    End Function
    Private Function GenLeader(ByRef StackStats As AllDataStructues.DesiredStats, ByRef DynStackStats As AllDataStructues.DesiredStats, _
                               ByRef FreeMeleeSlots As Integer, _
                               ByRef deltaLeadership As Integer, ByRef GroundTile As Boolean, ByRef NoLeader As Boolean, _
                               ByRef Bias As Double, ByRef LogID As Integer) As Integer
        If NoLeader Then Return -1

        Dim serialExecution As Boolean = (LogID < 0)
        Dim PossibleLeaders As New List(Of Integer)
        Dim SelectedLeader As Integer

        If ((DynStackStats.StackSize = 1 And DynStackStats.MaxGiants = 0) Or _
              (DynStackStats.StackSize = 2 And DynStackStats.MaxGiants = 1)) _
             AndAlso rndgen.Rand(0, 1, serialExecution) > 0.5 Then
            If DynStackStats.StackSize = 1 Then
                DynStackStats.StackSize += 1
                DynStackStats.MaxGiants += 1
            Else
                DynStackStats.StackSize += 1
            End If
        End If

        'Dim maxExpBar As Double = Math.Max(10000, 2 * DynStackStats.ExpBarAverage)
        Dim maxExpStrackKilled As Double = Math.Max(10000, 2 * DynStackStats.ExpStackKilled)

        'создаем список лидеров, которых вообще можем использовать
        Dim Tolerance As Double = 0.02 * (DynStackStats.StackSize - 1)
        Do While PossibleLeaders.Count < 3
            PossibleLeaders.Clear()
            For i As Integer = 0 To UBound(AllLeaders) Step 1
                If SelectPossibleLeader(i, Tolerance, DynStackStats, GroundTile) Then PossibleLeaders.Add(i)
            Next i
            If Tolerance > 2 And PossibleLeaders.Count > 0 Then Exit Do

            'If Tolerance * DynStackStats.ExpBarAverage > maxExpBar And Tolerance * DynStackStats.ExpStackKilled > maxExpStrackKilled Then
            If Tolerance * DynStackStats.ExpStackKilled > maxExpStrackKilled Then
                If DynStackStats.MaxGiants < 1 Then
                    DynStackStats.MaxGiants = 1
                    Tolerance = 0.02 * (DynStackStats.StackSize - 1)
                Else
                    If PossibleLeaders.Count > 0 Then Exit Do
                    Throw New Exception("Что-то не так в выборе возможных лидеров отряда" & vbNewLine & _
                                        "Имя локации: " & StackStats.LocationName & vbNewLine & _
                                        "StackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                        "DynStackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
                End If
            End If
            Tolerance += 0.2
        Loop

        Call AddToLog(LogID, AddressOf PrintSelectionList, AllLeaders, PossibleLeaders)
        Dim bar() As Double = SelectBar({DynStackStats.ExpBarAverage}, PossibleLeaders, {ExpBarLeaders}, Bias)
        SelectedLeader = comm.RandomSelection(PossibleLeaders, {ExpBarLeaders}, bar, _
                                              multLeaders, SigmaMultiplier(DynStackStats), serialExecution)

        If SelectedLeader = -1 Then
            Throw New Exception("Возможно, бесконечный цикл в случайном выборе из массива возможных лидеров" & vbNewLine & _
                                "Имя локации: " & StackStats.LocationName & vbNewLine & _
                                "StackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                "DynStackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
        End If

        'теперь нужно добрать воинов в отряд
        Dim R As Double = rndgen.Rand(0, 1, serialExecution)
        Dim leadershipCap As Integer = Math.Min(AllLeaders(SelectedLeader).leadership + deltaLeadership, 6)
        If AllLeaders(SelectedLeader).small Then
            leadershipCap = Math.Max(leadershipCap, 1)
        Else
            leadershipCap = Math.Max(leadershipCap, 2)
        End If
        If R < 0.1 Then
            If comm.BigStackUnits.ContainsKey(AllLeaders(SelectedLeader).unitID) Then
                If DynStackStats.StackSize > comm.BigStackUnits.Item(AllLeaders(SelectedLeader).unitID) Then
                    DynStackStats.StackSize -= 1
                End If
            Else
                DynStackStats.StackSize -= 1
            End If
        ElseIf R > 0.9 Then
            DynStackStats.StackSize += 1
            If DynStackStats.StackSize - DynStackStats.MeleeCount < secondrow.Length Then DynStackStats.MeleeCount += 1
        End If
        If AllLeaders(SelectedLeader).small Then
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 1)
        Else
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 2)
        End If
        DynStackStats.StackSize = Math.Min(DynStackStats.StackSize, leadershipCap)
        DynStackStats.MeleeCount = Math.Min(DynStackStats.MeleeCount, 3)
        DynStackStats.MaxGiants = Math.Min(DynStackStats.MaxGiants, 3)
        DynStackStats.ExpBarAverage = CInt((DynStackStats.ExpBarAverage * StackStats.StackSize) / DynStackStats.StackSize)

        Call ChangeLimit(AllLeaders, SelectedLeader, DynStackStats, FreeMeleeSlots, LogID)

        Return SelectedLeader
    End Function
    Private Function GenFingters(ByRef StackStats As AllDataStructues.DesiredStats, _
                                 ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                 ByRef FreeMeleeSlots As Integer, ByRef SelectedLeader As Integer, _
                                 ByRef GroundTile As Boolean, ByRef BaseStackSize As Integer, _
                                 ByRef Bias As Double, ByRef LogID As Integer) As List(Of Integer)
        Dim SelectedFighters As New List(Of Integer)
        Dim fighter As Integer
        Do While DynStackStats.StackSize > 0
            'создаем список воинов, которых можно использовать
            fighter = SelectFighters(False, False, DynStackStats, FreeMeleeSlots, SelectedLeader, _
                                     SelectedFighters, BaseStackSize, Bias, LogID)
            If fighter = -1 Then
                fighter = SelectFighters(True, False, DynStackStats, FreeMeleeSlots, SelectedLeader, _
                                         SelectedFighters, BaseStackSize, Bias, LogID)
                If fighter = -1 Then fighter = SelectFighters(True, True, DynStackStats, FreeMeleeSlots, SelectedLeader, _
                                                              SelectedFighters, BaseStackSize, Bias, LogID)
            End If
            If fighter = -1 Then
                If DynStackStats.MeleeCount > 0 Then
                    DynStackStats.MeleeCount = 0
                Else
                    Exit Do
                End If
            ElseIf fighter = -2 Then
                Throw New Exception("Возможно, бесконечный цикл в случайном выборе из массива возможных воинов" & vbNewLine & _
                                    "Имя локации: " & StackStats.LocationName & vbNewLine & _
                                    "StackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                    "DynStackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
            Else
                SelectedFighters.Add(fighter)
            End If
        Loop
        Return SelectedFighters
    End Function
    Private Function GenUnitsList(ByRef SelectedFighters As List(Of Integer), ByRef SelectedLeader As Integer, ByRef NoLeader As Boolean) As AllDataStructues.Unit()
        Dim SelectedUnits() As AllDataStructues.Unit
        If NoLeader Then
            ReDim SelectedUnits(SelectedFighters.Count - 1)
        Else
            ReDim SelectedUnits(SelectedFighters.Count)
        End If
        If Not NoLeader Then SelectedUnits(0) = AllDataStructues.Unit.Copy(AllLeaders(SelectedLeader))
        Dim n As Integer
        If NoLeader Then
            n = -1
        Else
            n = 0
        End If
        For Each i As Integer In SelectedFighters
            n += 1
            SelectedUnits(n) = AllDataStructues.Unit.Copy(AllFighters(i))
        Next i
        Return SelectedUnits
    End Function
    Private Function GenPositions(ByRef StackStats As AllDataStructues.DesiredStats, _
                                  ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                  ByRef SelectedUnits() As AllDataStructues.Unit) As AllDataStructues.Stack
        Dim result As New AllDataStructues.Stack With {.leaderPos = -1}
        ReDim result.pos(UBound(busytransfer)), result.level(UBound(busytransfer))
        Dim unitIsUsed(UBound(SelectedUnits)) As Boolean
        Dim firstRowSlots As Integer = 3
        Dim secondRowSlots As Integer = 3

        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And Not SelectedUnits(i).small Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And SelectedUnits(i).reach = 3 Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And Not SelectedUnits(i).reach = 3 Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, True, result)
            End If
        Next i
        For i As Integer = 0 To UBound(unitIsUsed) Step 1
            If Not unitIsUsed(i) Then Throw New Exception("Что-то не так в размещателе юнитов" & vbNewLine & _
                                                          "Имя локации: " & StackStats.LocationName & vbNewLine & _
                                                          "StackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                                          "DynStackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
        Next i
        For i As Integer = 0 To UBound(result.pos) Step 1
            If result.pos(i) = "" Then result.pos(i) = emptyItem
        Next i
        Return result
    End Function
    'Private Function GenPositions(ByRef SelectedUnits() As AllDataStructues.Unit, ByRef NoLeader As Boolean) As AllDataStructues.Stack
    '    Dim result As New AllDataStructues.Stack
    '    ReDim result.pos(UBound(busytransfer)), result.level(UBound(busytransfer))
    '    If NoLeader Then
    '        result.leaderPos = -1
    '    Else
    '        result.leaderPos = 0
    '        result.pos(0) = SelectedUnits(0).unitID
    '        result.level(0) = SelectedUnits(0).level
    '    End If
    '    For i As Integer = result.leaderPos + 1 To UBound(SelectedUnits) Step 1
    '        result.pos(i) = SelectedUnits(i).unitID
    '        result.level(i) = SelectedUnits(i).level
    '    Next i
    '    For i As Integer = SelectedUnits.Length To UBound(result.pos) Step 1
    '        If result.pos(i) = "" Then result.pos(i) = emptyItem
    '    Next i
    '    Return result
    'End Function
    Private Function SelectBar(ByRef Average() As Double, ByRef IDs As List(Of Integer), ByRef values()() As Double, ByRef bias As Double) As Double()
        Dim maxV, result(UBound(Average)) As Double
        For i As Integer = 0 To UBound(Average) Step 1
            maxV = 0
            For Each id As Integer In IDs
                maxV = Math.Max(maxV, values(i)(id))
            Next id
            result(i) = Average(i) + bias * (maxV - Average(i))
        Next i
        Return result
    End Function
    Private Function SelectStack(ByRef StackStats As AllDataStructues.DesiredStats, _
                                 ByRef DynStackStats() As AllDataStructues.DesiredStats) As Integer
        Dim possible1, possible2 As New List(Of Integer)
        Dim SizeTolerance As Integer = 0
        Do While possible1.Count < 0.5 * DynStackStats.Length
            possible1.Clear()
            SizeTolerance += 1
            For i As Integer = 0 To UBound(DynStackStats) Step 1
                If DynStackStats(i).StackSize <= SizeTolerance Then possible1.Add(i)
            Next i
        Loop
        Dim ExpTolerance As Double = 0
        Do While True
            possible2.Clear()
            ExpTolerance += 0.05
            For Each i As Integer In possible1
                If Math.Abs(DynStackStats(i).ExpStackKilled) <= ExpTolerance * StackStats.ExpStackKilled Then possible2.Add(i)
            Next i
            If possible2.Count >= 0.2 * DynStackStats.Length Then Exit Do
            If possible2.Count > 0 And ExpTolerance >= 0.1 Then Exit Do
        Loop
        Dim weight(UBound(DynStackStats)) As Double
        For Each i As Integer In possible2
            weight(i) = 1 / (1 + Math.Abs(DynStackStats(i).ExpStackKilled))
        Next i
        Dim selected As Integer = comm.RandomSelection(possible2, weight, False)
        Return selected
    End Function

    Private Function SelectFighters(ByRef skipfilter1 As Boolean, ByRef skipfilter2 As Boolean, _
                                    ByRef DynStackStats As AllDataStructues.DesiredStats, ByRef FreeMeleeSlots As Integer, _
                                    ByRef SelectedLeader As Integer, ByRef SelectedFighters As List(Of Integer), _
                                    ByRef BaseStackSize As Integer, _
                                    ByRef Bias As Double, ByRef LogID As Integer) As Integer

        Dim serialExecution As Boolean = (LogID < 0)
        Dim PossibleFighters As New List(Of Integer)
        'Dim TExpStack As Double = DynStackStats.ExpStackKilled / DynStackStats.StackSize
        Dim SelectedFighter As Integer
        'Dim nloops As Integer = 0
        'Do While PossibleFighters.Count = 0 'And TExpStack < 1.1 * DynStackStats.ExpStackKilled
        For j As Integer = 0 To UBound(AllFighters) Step 1
            If SelectPossibleFighter(skipfilter1, skipfilter2, j, DynStackStats, FreeMeleeSlots, _
                                     SelectedLeader, SelectedFighters, BaseStackSize) Then PossibleFighters.Add(j)
        Next j
        '    TExpStack += 0.1 * DynStackStats.ExpStackKilled / DynStackStats.StackSize
        '    nloops += 1
        '    If nloops > 10 Then Exit Do
        'Loop
        If PossibleFighters.Count > 0 Then
            Call AddToLog(LogID, AddressOf PrintSelectionList, AllFighters, PossibleFighters)

            Dim bar() As Double = SelectBar({DynStackStats.ExpBarAverage, CDbl(DynStackStats.ExpStackKilled) / CDbl(DynStackStats.StackSize)}, _
                                            PossibleFighters, {ExpBarFighters, ExpKilledFighters}, Bias)
            SelectedFighter = comm.RandomSelection(PossibleFighters, {ExpBarFighters, ExpKilledFighters}, _
                                                   bar, multFighters, SigmaMultiplier(DynStackStats), serialExecution)
            If SelectedFighter = -1 Then Return -2
            Call ChangeLimit(AllFighters, SelectedFighter, DynStackStats, FreeMeleeSlots, LogID)
        Else
            SelectedFighter = -1
        End If
        Return SelectedFighter
    End Function
    Private Function SelectPossibleFighter(ByRef skipMaxGiantsFilter As Boolean, _
                                           ByRef skipRangeFilter As Boolean, _
                                           ByRef fighterID As Integer, _
                                           ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                           ByRef FreeMeleeSlots As Integer, _
                                           ByRef SelectedLeader As Integer, _
                                           ByRef SelectedFighters As List(Of Integer), _
                                           ByRef BaseStackSize As Integer) As Boolean
        If comm.SoleUnits.ContainsKey(AllFighters(fighterID).unitID) Then
            Dim sole As List(Of String) = comm.SoleUnits.Item(AllFighters(fighterID).unitID)
            If SelectedLeader > -1 AndAlso sole.Contains(AllLeaders(SelectedLeader).unitID) Then Return False
            For Each id As Integer In SelectedFighters
                If sole.Contains(AllFighters(id).unitID) Then Return False
            Next id
        End If
        If Not DynStackStats.Race.Contains(AllFighters(fighterID).race) Then Return False

        If comm.BigStackUnits.ContainsKey(AllFighters(fighterID).unitID) _
        AndAlso BaseStackSize < comm.BigStackUnits.Item(AllFighters(fighterID).unitID) Then Return False

        'Dim mult As Double
        'If AllFighters(fighterID).small Then
        '    mult = 1
        'Else
        '    mult = 2
        'End If
        'If AllFighters(fighterID).EXPkilled > mult * DynStackStats.ExpStackKilled / DynStackStats.StackSize Then Return False
        If DynStackStats.ExpStackKilled <= 0 Then Return False
        If AllFighters(fighterID).EXPkilled > 1.15 * DynStackStats.ExpStackKilled + 15 Then Return False
        If Not AllFighters(fighterID).small Then
            If DynStackStats.MaxGiants = 0 And Not skipMaxGiantsFilter Then Return False
            If DynStackStats.StackSize < 2 Then Return False
            If FreeMeleeSlots = 0 Then Return False
        End If

        If AllFighters(fighterID).reach = 3 Then
            If DynStackStats.MeleeCount = 0 Then Return False
            If FreeMeleeSlots = 0 Then Return False
        Else
            If Not skipRangeFilter And DynStackStats.MeleeCount > 0 Then Return False
        End If
        Return True
    End Function
    Private Function PrintSelectionList(ByRef units() As AllDataStructues.Unit, ByRef possible As List(Of Integer)) As String
        Dim result As String = ""
        For Each id As Integer In possible
            If Not result = "" Then result &= " "
            result &= units(id).name
        Next id
        Return "Selection pool:" & vbNewLine & result
    End Function

    Private Function SetUnitPosition(ByRef i As Integer, ByRef units() As AllDataStructues.Unit, _
                                     ByRef FRowSlots As Integer, ByRef SRowSlots As Integer, _
                                     ByRef AnySlot As Boolean, ByRef result As AllDataStructues.Stack) As Boolean
        Dim placed As Boolean = False
        Dim n1 As Integer = rndgen.RndPos(FRowSlots, True)
        Dim n2 As Integer = rndgen.RndPos(SRowSlots, True)

        Dim t As Integer
        Dim m As Integer = 0
        For k As Integer = 0 To UBound(firstrow) Step 1
            If Not units(i).small Or units(i).reach = 3 Then
                If result.pos(firstrow(k)) = "" Then
                    m += 1
                    If m = n1 Then
                        Call SetUnitPos(result, firstrow(k), units(i))
                        FRowSlots -= 1
                        If Not units(i).small Then
                            result.pos(busytransfer(firstrow(k))) = emptyItem
                            SRowSlots -= 1
                        End If
                        If i = 0 Then result.leaderPos = firstrow(k)
                        placed = True
                        Exit For
                    End If
                End If
            ElseIf AnySlot Then
                For p As Integer = 0 To 1 Step 1
                    t = 2 * k + p
                    If result.pos(t) = "" Then
                        m += 1
                        If m = n1 + n2 Then
                            Call SetUnitPos(result, t, units(i))
                            For r As Integer = 0 To UBound(firstrow) Step 1
                                If t = firstrow(r) Then
                                    FRowSlots -= 1
                                    If i = 0 Then result.leaderPos = firstrow(r)
                                    p = 1
                                    placed = True
                                    Exit For
                                ElseIf t = secondrow(r) Then
                                    SRowSlots -= 1
                                    If i = 0 Then result.leaderPos = secondrow(r)
                                    p = 1
                                    placed = True
                                    Exit For
                                End If
                            Next r
                        End If
                    End If
                Next p
            Else
                If result.pos(secondrow(k)) = "" Then
                    m += 1
                    If m = n2 Then
                        Call SetUnitPos(result, secondrow(k), units(i))
                        SRowSlots -= 1
                        If i = 0 Then result.leaderPos = secondrow(k)
                        placed = True
                        Exit For
                    End If
                End If
            End If
        Next k
        Return placed
    End Function
    Private Sub SetUnitPos(ByRef result As AllDataStructues.Stack, ByRef pos As Integer, ByRef unit As AllDataStructues.Unit)
        result.pos(pos) = unit.unitID
        result.level(pos) = unit.level
    End Sub

    Private Sub ChangeLimit(ByRef List() As AllDataStructues.Unit, ByRef id As Integer, _
                            ByRef DynStackStats As AllDataStructues.DesiredStats,
                            ByRef FreeMeleeSlots As Integer, ByRef LogID As Integer)
        DynStackStats.ExpStackKilled -= List(id).EXPkilled

        If List(id).small And DynStackStats.StackSize > 1 Then
            DynStackStats.ExpBarAverage = Math.Max(CInt((DynStackStats.StackSize * DynStackStats.ExpBarAverage - List(id).EXPnext) / CDbl(DynStackStats.StackSize - 1)), 10)
        ElseIf Not List(id).small And DynStackStats.StackSize > 2 Then
            DynStackStats.ExpBarAverage = Math.Max(CInt((DynStackStats.StackSize * DynStackStats.ExpBarAverage - List(id).EXPnext) / CDbl(DynStackStats.StackSize - 2)), 10)
        End If

        If Not List(id).small Then
            DynStackStats.MaxGiants -= 1
            DynStackStats.StackSize -= 2
            FreeMeleeSlots -= 1
        ElseIf List(id).reach = 3 Then
            DynStackStats.StackSize -= 1
            DynStackStats.MeleeCount = Math.Max(0, DynStackStats.MeleeCount - 1)
            FreeMeleeSlots = Math.Max(0, FreeMeleeSlots - 1)
        Else
            DynStackStats.StackSize -= 1
        End If
        DynStackStats.MeleeCount = Math.Min(DynStackStats.MeleeCount, FreeMeleeSlots)

        Call AddToLog(LogID, "Unit added: " & List(id).name & " id: " & List(id).unitID)
        Call AddToLog(LogID, DynStackStats, True)
    End Sub

End Class

Public Class RndValueGen
    Private betTick, lastRAM As Double
    Private tempPat, delimiterBias As Integer

    Public Sub New()
        For i As Integer = 0 To 10 Step 1
            Call RndDbl()
        Next i
    End Sub

    Private Function RndDbl() As Double
        If betTick = 0 Or System.Double.IsInfinity(betTick) Then
            betTick = (4 + 0.5 * delimiterBias + Math.Pow(delimiterBias + 1, 3))
        End If
        betTick = (Now.Ticks - betTick) * (4 + delimiterBias) * 0.112
        Dim tick As Double = betTick
        If tick < 0 Then tick = -tick
        If delimiterBias > 3 Then delimiterBias = 0
        If Double.IsInfinity(tick) Then
            tick = Math.Pow(3.1 + 0.05 * CDbl(delimiterBias), _
                            Math.Min(15, 3 * delimiterBias))
        End If
        If tick < 1000 Then tick += Math.Pow(2, 1 + delimiterBias)
        tick = tick * Math.Pow(9.999, 19 - CInt(Math.Log10(tick)))
        Dim RAM As Double
        For i As Integer = 0 To 40 Step 1
            RAM = (RAM + Threading.Thread.VolatileRead(tick + i)) * 0.5
        Next i
        'RAM = Math.Abs(RAM)
        RAM = RAM * Math.Pow(10, 15 - CInt(Math.Log10(RAM)))
        RAM = CDbl(Mid(((lastRAM + RAM) / 2).ToString, 5))
        RAM = RAM * Math.Pow(10, 15 - CInt(Math.Log10(RAM))) + 0.1
        lastRAM = RAM

        tempPat += 1
        If tempPat > 20 Then tempPat = 1
        If delimiterBias > 3 Then delimiterBias = 0

        Dim c1 As Double = RAM / (tempPat + delimiterBias)
        Dim c2 As Double = (tick * (c1 + 1)) / ((100 + Math.Pow(4, 1 + delimiterBias)) * c1)
        c2 *= Math.Pow(10, CInt(Math.Log10(c1)) - CInt(Math.Log10(c2)))
        Dim c3 As Double = Math.Pow(9.1 + delimiterBias, 6 + delimiterBias) _
            * Math.Sqrt(CDbl(Threading.Thread.CurrentThread.ManagedThreadId) + delimiterBias) _
            * c1 / c2
        Dim c12sum As Double = c1 + c2
        c3 *= Math.Pow(10, CInt(Math.Log10(c12sum * 0.5)) - CInt(Math.Log10(c3)))
        'Dim ws As long = Environment.WorkingSet
        Dim c4 As Double = Math.Pow(487 + delimiterBias + tempPat, _
                                    (0.5 * c12sum / (c1 + c3))) * c12sum / c3
        c4 *= Math.Pow(10, CInt(Math.Log10((c12sum + c3) * 0.3333)) - CInt(Math.Log10(c4)))
        Dim c5 As Double = Math.Pow((c12sum + c3) / (c12sum + c4), c3 / c4)
        c5 *= Math.Pow(10, CInt(Math.Log10((c12sum + c3 + c4) * 0.25)) - CInt(Math.Log10(c5)))

        If delimiterBias > 3 Then delimiterBias = 0
        Dim v As Double = 0.2 * (c12sum + c3 + c4 + c5) * Math.Pow(0.1, 5 + delimiterBias)
        delimiterBias += 1
        Return v - Math.Floor(v)
    End Function
    '''<summary>Returns random value with uniform distribution.</summary>
    '''<param name="lower">Minimum value.</param>
    '''<param name="upper">Maximum value.</param>
    Public Function Rand(ByVal lower As Double, ByVal upper As Double) As Double
        Dim r As Double = RndDbl()
        Return lower + r * (upper - lower)
    End Function
    '''<summary>Returns random value with uniform distribution. Use this to obtain more uniform distribution in the case of serial code.</summary>
    '''<param name="lower">Minimum value.</param>
    '''<param name="upper">Maximum value.</param>
    Public Function PRand(ByVal lower As Double, ByVal upper As Double) As Double
        Dim value(Environment.ProcessorCount - 1), l, u As Double
        Dim n As Integer = CInt(Math.Round(Rand(0, Environment.ProcessorCount - 1), 0))
        l = lower : u = upper
        Parallel.For(0, Environment.ProcessorCount, _
         Sub(i As Integer)
             value(i) = Rand(l, u)
         End Sub)
        Return value(n)
    End Function

    '''<summary>Returns random value with uniform distribution.</summary>
    '''<param name="lower">Minimum value.</param>
    '''<param name="upper">Maximum value.</param>
    ''' <param name="serial">True, if use in serial code</param>
    Friend Function Rand(ByRef lower As Double, ByRef upper As Double, ByRef serial As Boolean) As Double
        If serial Then
            Return PRand(lower, upper)
        Else
            Return Rand(lower, upper)
        End If
    End Function

    '''<summary>Returns random value with uniform distribution from 1 to n.</summary>
    ''' <param name="n">Value greater than zero</param>
    ''' <param name="serial">True, if use in serial code</param>
    Friend Function RndPos(ByRef n As Integer, ByRef serial As Boolean) As Integer
        Dim R As Double = Rand(0, 1, serial)
        Dim dr As Double = 1 / n
        For i As Integer = 1 To n Step 1
            If CDbl(i) * dr >= R Then Return i
        Next i
        Return n
    End Function

End Class

Public Class Common

    Public rndgen As New RndValueGen
    Private Races As New Dictionary(Of String, Integer)
    Friend RaceNumberToRaceChar As New Dictionary(Of Integer, String)
    ''' <summary>Список исключаемых объектов</summary>
    Public excludedObjects As New List(Of String)
    ''' <summary>Список параметров отрядов с описанием</summary>
    Public StatFields() As AllDataStructues.StackStatsField
    ''' <summary>Расы юнитов, назначаемые независимо от ресурсов игры</summary>
    Public customRace As New Dictionary(Of String, String)
    ''' <summary>Допустимые расы локаций и поверхности, на которых можн ставить объекты</summary>
    Public objectRace As New Dictionary(Of String, DecorationPlacingProperties)
    ''' <summary>Описывает то, как цеплять друг к другу "Плато" и "Водопады"</summary>
    Public PlateauConstruction As New Dictionary(Of String, String)
    ''' <summary>Расы лордов</summary>
    Public LordsRace As New Dictionary(Of String, Integer)
    ''' <summary>Множитель шанса появления предмета</summary>
    Public LootItemChanceMultiplier As New Dictionary(Of String, Double)
    ''' <summary>Ключ - ID юнита, значение - ID юнитов, с которыми он не должен быть в одном отряде</summary>
    Public SoleUnits As New Dictionary(Of String, List(Of String))
    ''' <summary>Ключ - ID юнита, значение - минимальный размер стэка для юнита</summary>
    Public BigStackUnits As New Dictionary(Of String, Integer)
    ''' <summary>Ключ - ID типа предмета, значение - тип предмета</summary>
    Public itemType As New Dictionary(Of Integer, String)
    ''' <summary>Ключ - тип предмета, значение - ID типа предмета</summary>
    Public itemTypeID As New Dictionary(Of String, Integer)

    Friend valConv As New ValueConverter

    Friend ConsumableItemsTypes, NonconsumableItemsTypes, JewelItemsTypes As New List(Of Integer)
    Friend ItemTypesLists() As List(Of Integer)

    Public Sub New()
        Dim splitedRace() As String = TxtSplit(My.Resources.Races)
        Dim srow() As String
        For Each item As String In splitedRace
            srow = item.Split(CChar(" "))
            For i As Integer = 0 To UBound(srow) Step 1
                Races.Add(srow(i).ToUpper, CInt(srow(UBound(srow))))
            Next i
            RaceNumberToRaceChar.Add(CInt(srow(UBound(srow))), srow(1).ToUpper)
        Next item
        Dim splitedFields() As String = TxtSplit(My.Resources.StackStatsFields)
        ReDim StatFields(CInt(splitedFields.Length / 2 - 1))

        Dim racesList As String = ""
        splitedRace = My.Resources.Races.Replace(Chr(10), Chr(13)).Split(Chr(13))
        For i As Integer = 0 To UBound(splitedRace) Step 1
            If splitedRace(i).Length > 1 AndAlso Not splitedRace(i).Substring(0, 1) = "#" Then
                racesList &= splitedRace(i) & vbNewLine
            End If
        Next i
        Dim valConv As New ValueConverter
        Dim k As Integer
        For i As Integer = 0 To UBound(splitedFields) Step 2
            k = CInt(i / 2)
            StatFields(k).description = splitedFields(i)
            StatFields(k).name = splitedFields(i + 1)
            StatFields(k).description = StatFields(k).description.Replace("$jm$", valConv.JewelItemsCostDevider.ToString)
            StatFields(k).description = StatFields(k).description.Replace("$gm$", valConv.giantUnitsExpMultiplicator.ToString)
            StatFields(k).description = StatFields(k).description.Replace("$ri$", racesList)
            StatFields(k).description = StatFields(k).description.Replace("$newline$", vbNewLine)
        Next i

        Dim lords() As String = TxtSplit(My.Resources.Lords)
        For i As Integer = 0 To UBound(lords) Step 1
            Dim s() As String = lords(i).Split(CChar(" "))
            LordsRace.Add(s(0).ToUpper, RaceIdentifierToSubrace(s(1)))
        Next i

        ConsumableItemsTypes.AddRange(New Integer() {4, 5, 6, 7, 8, 11, 12})
        NonconsumableItemsTypes.AddRange(New Integer() {0, 1, 2, 3, 9, 13})
        JewelItemsTypes.AddRange(New Integer() {10})
        ItemTypesLists = {ConsumableItemsTypes, NonconsumableItemsTypes, JewelItemsTypes}

        Dim splitedItemsTypes() As String = TxtSplit(My.Resources.Items)
        For i As Integer = 0 To UBound(splitedItemsTypes) Step 1
            srow = splitedItemsTypes(i).Split(CChar(" "))
            itemType.Add(CInt(srow(0)), srow(1).ToUpper)
            itemTypeID.Add(srow(1).ToUpper, CInt(srow(0)))
        Next i

    End Sub

    ''' <summary>Передаст в лог содержимое excludedObjects, customRace, objectRace, LootItemChanceMultiplier, SoleUnits</summary>
    ''' <param name="log">Сюда будем писать данные</param>
    ''' <param name="rStack">Инициализированный класс</param>
    Public Sub PrintResourcesToLog(ByRef log As Log, ByRef rStack As RandStack)
        If Not log.IsEnabled Then Exit Sub

        Dim Races As New Dictionary(Of String, String)
        Dim t() As String = TxtSplit(My.Resources.Races)
        For Each s As String In t
            Dim r() As String = s.Split(CChar(" "))
            For i As Integer = 0 To UBound(r) Step 1
                Races.Add(r(i).ToUpper, r(0))
            Next i
        Next s

        Dim result, name As String
        result = vbNewLine & "----Excluded objects list----"
        For Each item As String In excludedObjects
            name = rStack.FindUnitStats(item).name
            If name = "" Then name = rStack.FindItemStats(item).name
            If name = "" AndAlso rStack.comm.itemType.ContainsValue(item.ToUpper) Then name = "item type"
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & name
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Custom units races list----"
        For Each item As String In customRace.Keys
            name = rStack.FindUnitStats(item).name
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & Races.Item(customRace.Item(item).ToUpper) & " - " & name
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Custom objects races list----"
        For Each item As String In objectRace.Keys
            name = "Map object"
            result &= vbNewLine & item & " - " & Races.Item(customRace.Item(item).ToUpper) & " - " & name
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Loot item chance multipliers list----"
        For Each item As String In LootItemChanceMultiplier.Keys
            name = rStack.FindItemStats(item).name
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & LootItemChanceMultiplier.Item(item) & " - " & name
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Sole units list----"
        For Each item As String In SoleUnits.Keys
            name = rStack.FindUnitStats(item).name
            If name = "" Then name = "???"
            result &= vbNewLine & item & " - " & name & " // "
            Dim s As String = ""
            For Each u As String In SoleUnits.Item(item)
                name = rStack.FindUnitStats(u).name
                If name = "" Then name = "???"
                If Not s = "" Then s &= " # "
                s &= u & " - " & name
            Next u
            result &= s
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Big stack units list----"
        For Each item As String In BigStackUnits.Keys
            name = rStack.FindUnitStats(item).name
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & name & " - " & BigStackUnits.Item(item)
        Next item
        Call log.Add(result)
    End Sub

    ''' <summary>Читает и парсит файл с параметрами генерируемых отрядов.
    ''' Не важен порядок полей и регистр.
    ''' Не проверяет корректность данных по типу</summary>
    ''' <param name="path">Путь к файлу. Не проверяет, существует ли файл.
    ''' Если path=%testfile%, то распарсит теастовый файл.
    ''' Если path=%default%, то вернет значения, устанавливаемые для пропущенных полей.</param>
    Public Function ParseDesiredStackStatsFile(ByRef path As String) As AllDataStructues.DesiredStats()
        Dim txt(), s(), r(), fu As String
        Dim defaultStats As New AllDataStructues.DesiredStats With {.ExpBarAverage = 200, .ExpStackKilled = 75, _
                                                                    .MeleeCount = 2, .Race = New List(Of Integer), .StackSize = 2, _
                                                                    .shopContent = Nothing}
        defaultStats.Race.Add(1)
        If Not path = My.Resources.testFileKeyword.ToLower Then
            txt = TxtSplit(IO.File.ReadAllText(path).Replace("=", vbTab))
        ElseIf path = My.Resources.readDefaultFileKeyword.ToLower Then
            Return New AllDataStructues.DesiredStats() {defaultStats}
        Else
            txt = TxtSplit(My.Resources.TestStackStats.Replace("=", vbTab))
        End If
        Dim addedLabels As New List(Of String)
        Dim result(UBound(txt)) As AllDataStructues.DesiredStats
        For i As Integer = 0 To UBound(txt) Step 1
            s = txt(i).Split(CChar(" "))
            result(i) = AllDataStructues.DesiredStats.Copy(defaultStats)
            For f As Integer = 0 To UBound(s) Step 2
                fu = s(f).ToUpper
                For k As Integer = 0 To UBound(StatFields) Step 1
                    If fu = StatFields(k).name.ToUpper Then
                        If k = 0 Then
                            result(i).LocationName = s(f + 1) 'ID
                            If Not addedLabels.Contains(result(i).LocationName) Then
                                addedLabels.Add(result(i).LocationName)
                            Else
                                Throw New Exception("В файле с параметрами отрядов есть повторяющееся имя локации: " & result(i).LocationName)
                            End If
                        ElseIf k = 1 Then
                            result(i).ExpBarAverage = Math.Max(ValueConverter.StrToInt(s(f + 1), txt(i), s(f)), 10) 'AverageExpBar
                        ElseIf k = 2 Then
                            result(i).ExpStackKilled = Math.Max(ValueConverter.StrToInt(s(f + 1), txt(i), s(f)), 5) 'ExpStackKilled
                        ElseIf k = 3 Then
                            Dim rid As Integer
                            r = s(f + 1).Split(CChar("+")) 'Race
                            result(i).Race.Clear()
                            For n As Integer = 0 To UBound(r) Step 1
                                rid = RaceIdentifierToSubrace(r(n))
                                If Not result(i).Race.Contains(rid) Then result(i).Race.Add(rid)
                            Next n
                        ElseIf k = 4 Then
                            result(i).StackSize = Math.Max(ValueConverter.StrToInt(s(f + 1), txt(i), s(f)), 1) 'StackSize
                        ElseIf k = 5 Then
                            result(i).MaxGiants = Math.Max(ValueConverter.StrToInt(s(f + 1), txt(i), s(f)), 0) 'MaxGiants
                        ElseIf k = 6 Then
                            result(i).MeleeCount = Math.Max(ValueConverter.StrToInt(s(f + 1), txt(i), s(f)), 0) 'MeleeSlots
                        ElseIf k = 7 Then
                            result(i).LootCost = Math.Max(ValueConverter.StrToInt(s(f + 1), txt(i), s(f)), 0) 'LootCost
                        ElseIf k = 8 Then
                            result(i).IGen.ConsumableItems = AllDataStructues.ItemGenSettings.Read(s(f + 1)) 'CItemsGen
                        ElseIf k = 9 Then
                            result(i).IGen.NonconsumableItems = AllDataStructues.ItemGenSettings.Read(s(f + 1)) 'NItemsGen
                        ElseIf k = 10 Then
                            result(i).IGen.JewelItems = AllDataStructues.ItemGenSettings.Read(s(f + 1)) 'JItemsGen
                        ElseIf k = 11 Then
                            r = s(f + 1).Split(CChar("+")) 'ShopContent
                            result(i).shopContent = New List(Of String)
                            For n As Integer = 0 To UBound(r) Step 1
                                result(i).shopContent.Add(r(n).ToUpper)
                            Next n
                        ElseIf k = 12 Then
                            result(i).isInternalCityGuard = ValueConverter.StrToBool(s(f + 1)) 'IsInternalCityGuard
                        End If
                    End If
                Next k
            Next f
        Next i
        Return result
    End Function
    
    ''' <summary>Сохраняет в файл параметры генерируемых отрядов</summary>
    ''' <param name="path">Путь к файлу</param>
    ''' <param name="content">Параметры</param>
    Public Sub WriteDesiredStackStats(ByRef path As String, ByRef content() As AllDataStructues.DesiredStats)
        Dim s(UBound(content)) As String
        For i As Integer = 0 To UBound(s) Step 1
            s(i) = TxtSplit(AllDataStructues.DesiredStats.Print(content(i), RaceNumberToRaceChar).Replace(vbNewLine, vbTab))(0)
        Next i
        If Not path = My.Resources.testFileKeyword Then
            IO.File.WriteAllLines(path, s)
        Else
            path = ""
            For i As Integer = 0 To UBound(s) Step 1
                path &= s(i) & vbNewLine
            Next i
        End If
    End Sub

    ''' <summary>Возвращает ID расы, соответствующее файлам игры</summary>
    ''' <param name="ID">Идентификатор расы (файл races.txt)</param>
    Public Function RaceIdentifierToSubrace(ByVal ID As String) As Integer
        Dim uID As String = ID.ToUpper
        If Races.ContainsKey(uID) Then
            Return Races.Item(uID)
        Else
            Throw New Exception("Неизвестный идентификатор расы:" & ID)
            Return -1
        End If
    End Function
    ''' <summary>Возвращает ID расы, соответствующее файлам игры</summary>
    ''' <param name="ID">Идентификатор расы (файл races.txt)</param>
    Public Function RaceIdentifierToSubrace(ByVal ID As Integer) As Integer
        Return RaceIdentifierToSubrace(ID.ToString)
    End Function

    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    Public Function TxtSplit(ByVal TXT As String) As String()
        Dim splited() As String = TXT.Replace(Chr(10), Chr(13)).Replace(vbTab, " ").Split(Chr(13))
        Dim parseString(UBound(splited)) As Boolean
        Dim nStrings As Integer = -1
        For i As Integer = 0 To UBound(splited) Step 1
            If splited(i).Length > 1 AndAlso Not splited(i).Substring(0, 1) = "#" Then
                parseString(i) = True
                nStrings += 1
            End If
        Next i
        If nStrings = -1 Then Return Nothing
        Dim result(nStrings) As String
        nStrings = -1
        Dim L0 As Integer
        For i As Integer = 0 To UBound(splited) Step 1
            If parseString(i) Then
                L0 = 0
                Do While Not L0 = splited(i).Length
                    L0 = splited(i).Length
                    splited(i) = splited(i).Replace("  ", " ")
                Loop
                If Not splited(i) = " " Then
                    If splited(i).Substring(0, 1) = " " Then splited(i) = splited(i).Substring(1)
                    If splited(i).Substring(splited(i).Length - 1) = " " Then splited(i) = splited(i).Substring(0, splited(i).Length - 1)
                    nStrings += 1
                    result(nStrings) = splited(i)
                End If
            End If
        Next i
        If nStrings = -1 Then Return Nothing
        If Not nStrings = UBound(result) Then ReDim Preserve result(nStrings)
        Return result
    End Function

    ''' <summary>Dыбирает случайным образом запись из списка</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор.
    ''' Если массивы Stats (v) и DesiredStats (d) инициализированы, то каждой записи на их основании будет присвоен стат. вес в соответствие с распределением Гаусса g(v,d).
    ''' Если Stats (v1, v2, v3...) содержит несколько массивов, то DesiredStats (d1, d2, d3...) должен содержать соответствующее кол-во значений, и в этом случае для получения полного стат. веса стат. веса от каждого параметра будут перемножены g=g(v1,d1)*g(v2,d2)*g(v3,d3)...
    ''' Если же оба массива не инициализированы, то у всех записей будет одинаковый стат. вес</param>
    ''' <param name="Stats">Массивы параметров, по которым выбираем запись</param>
    ''' <param name="DesiredStats">Какое значение будет иметь наибольший стат. вес (по одному на каждый массив значений)</param>
    ''' <param name="mult">Множитель "желаемого" значения, отражающий особенности выбираемого объекта (например, размер юнита или тип предмета).
    ''' Если не инициализирован, то считается что множитель для всех записей равен единице</param>
    ''' <param name="BaseSmearing">Множитель для Сигмы в распределении Гаусса. Сигма=Множитель*Желаемое_значение</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByRef mult() As Double, _
                                    ByVal BaseSmearing As Double, ByVal serial As Boolean) As Integer
        Dim noValue As Boolean = False
        If IsNothing(Stats) And IsNothing(DesiredStats) Then
            noValue = True
        ElseIf Not IsNothing(Stats) = IsNothing(DesiredStats) Then
            Throw New Exception("RandomSelection: Только один из массивов инициализирован")
            Return -1
        Else
            For i As Integer = 1 To UBound(Stats) Step 1
                If Not Stats(0).Length = Stats(i).Length Then
                    Throw New Exception("RandomSelection: Массивы статов должны иметь одинаковую длину")
                    Return -1
                End If
            Next i
            If Not Stats.Length = DesiredStats.Length Then
                Throw New Exception("RandomSelection: Количество массивов статов должно соответствовать количеству ""желаемых"" статов")
                Return -1
            End If
            If Not IsNothing(mult) AndAlso Not Stats(0).Length = mult.Length Then
                Throw New Exception("RandomSelection: Если массив множителей инициализирован, то он должен иметь одинаковую длину с массивами статов")
                Return -1
            End If
        End If

        Dim WeightsSum As Double = 0
        Dim Weight() As Double
        Dim smearing As Double = 0
        Dim m As Double

        If noValue Then
            ReDim Weight(IDs.Max)
        Else
            ReDim Weight(UBound(Stats(0)))
            If BaseSmearing <= 0 Then
                Throw New Exception("RandomSelection: BaseSmearing должно быть положительным числом")
                Return -1
            End If
        End If
        Dim maxSmearing As Double = Math.Max(10 * BaseSmearing, 10)
        Do While WeightsSum = 0
            smearing += BaseSmearing
            For Each i As Integer In IDs
                Weight(i) = 1
                If Not noValue Then
                    If Not IsNothing(mult) Then
                        m = mult(i)
                    Else
                        m = 1
                    End If
                    For j As Integer = 0 To UBound(Stats) Step 1
                        Weight(i) *= Gauss(Stats(j)(i), m * DesiredStats(j), smearing)
                    Next j
                End If
                WeightsSum += Weight(i)
            Next i
            If smearing > maxSmearing Then Return -1
        Loop
        Return RandomSelection(IDs, Weight, serial)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор.
    ''' Если массивы Stats (v) и DesiredStats (d) инициализированы, то каждой записи на их основании будет присвоен стат. вес в соответствие с распределением Гаусса g(v,d).
    ''' Если Stats (v1, v2, v3...) содержит несколько массивов, то DesiredStats (d1, d2, d3...) должен содержать соответствующее кол-во значений, и в этом случае для получения полного стат. веса стат. веса от каждого параметра будут перемножены g=g(v1,d1)*g(v2,d2)*g(v3,d3)...
    ''' Если же оба массива не инициализированы, то у всех записей будет одинаковый стат. вес</param>
    ''' <param name="Stats">Массивы параметров, по которым выбираем запись</param>
    ''' <param name="DesiredStats">Какое значение будет иметь наибольший стат. вес (по одному на каждый массив значений)</param>
    ''' <param name="BaseSmearing">Множитель для Сигмы в распределении Гаусса. Сигма=Множитель*Желаемое_значение</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByVal BaseSmearing As Double, _
                                    ByVal serial As Boolean) As Integer
        Return RandomSelection(IDs, Stats, DesiredStats, Nothing, BaseSmearing, serial)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка, считая, что у всех записей будет одинаковый стат. вес</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), _
                                    ByVal serial As Boolean) As Integer
        Return RandomSelection(IDs, Nothing, Nothing, 0, serial)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор.
    ''' Если массив Weight не инициализирован, то у всех записей будет одинаковый стат. вес</param>
    ''' <param name="Weight">Вероятность выбрать запись прямо пропорциональна величине стат. веса. Сумма весов может быть не равна единице</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Weight() As Double, _
                                    ByVal serial As Boolean) As Integer
        Dim tWeight() As Double
        If IsNothing(Weight) Then
            ReDim tWeight(IDs.Max)
            For Each i In IDs
                tWeight(i) = 1
            Next i
        Else
            tWeight = Weight
        End If
        Dim WeightsSum As Double
        For Each i As Integer In IDs
            WeightsSum += tWeight(i)
        Next i
        Dim R As Double = rndgen.Rand(0, WeightsSum, serial)
        Dim W As Double = 0
        Dim SelectedItem As Integer = -1
        For Each i In IDs
            If tWeight(i) < 0 Then Throw New Exception("Отрицательный стат вес")
            W += tWeight(i)
            If W > R Then
                SelectedItem = i
                Exit For
            End If
        Next i
        If SelectedItem = -1 Then SelectedItem = IDs.Item(IDs.Count - 1)
        Return SelectedItem
    End Function
    ''' <summary>e^(-0.5 * ((X - avX) / (sigma * avX)) ^ 2)</summary>
    Public Function Gauss(ByVal X As Double, ByVal avX As Double, ByVal sigma As Double) As Double
        Return Math.Exp(-0.5 * ((X - avX) / (sigma * avX)) ^ 2)
    End Function

    Delegate Sub readFunction(ByRef paths() As String)
    ''' <summary>Читает список юнитов и предметов, которые не должен использовать генератор</summary>
    ''' <param name="ExcludeLists">Файлы со списками исключенных объектов. Записи в них могут повторяться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла).
    ''' Для чтения из листа сюжетных юнитов из Ванили в массив нужно добавить строчку %novanillalore% (наличие этого ключевого в файле запустит чтение дефолтного файла).
    ''' Для чтения из листа сюжетных юнитов из Мода в массив нужно добавить строчку %nomodlore% (наличие этого ключевого в файле запустит чтение дефолтного файла).</param>
    Public Sub ReadExcludedObjectsList(ByRef ExcludeLists() As String)
        If IsNothing(ExcludeLists) Then Exit Sub
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword, My.Resources.readMLoreFileKeyword, My.Resources.readVLoreFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.ExcludeIDs, My.Resources.ExcludeIDs_ModLore, My.Resources.ExcludeIDs_VanillaLore}
        For i As Integer = 0 To UBound(ExcludeLists) Step 1
            s = prepareToFileRead(ExcludeLists(i), defaultKeys, defaultVals)
            Call ReadFile(1, s, ExcludeLists(i), AddressOf ReadExcludedObjectsList, defaultKeys)
        Next i
    End Sub
    ''' <summary>Читает множители шанса выпадения для отдельных предметов</summary>
    ''' <param name="MultipliersList">Файлы с множителями шанса появления определенных предметов.
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub ReadLootItemChanceMultiplier(ByRef MultipliersList() As String)
        If IsNothing(MultipliersList) Then Exit Sub
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.LootItemChanceMultiplier}
        For i As Integer = 0 To UBound(MultipliersList) Step 1
            s = prepareToFileRead(MultipliersList(i), defaultKeys, defaultVals)
            Call ReadFile(5, s, MultipliersList(i), AddressOf ReadLootItemChanceMultiplier, defaultKeys)
        Next i
    End Sub
    ''' <summary>Читает список, переопределяющий расы нужных юнитов</summary>
    ''' <param name="CustomUnitRace">Файлы со списками рас юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub ReadCustomUnitRace(ByRef CustomUnitRace() As String)
        If IsNothing(CustomUnitRace) Then Exit Sub
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.UnitRace}
        For i As Integer = 0 To UBound(CustomUnitRace) Step 1
            s = prepareToFileRead(CustomUnitRace(i), defaultKeys, defaultVals)
            Call ReadFile(2, s, CustomUnitRace(i), AddressOf ReadCustomUnitRace, defaultKeys)
        Next i
    End Sub
    ''' <summary>Читает список юнитов, которые должны находиться в отряде в единственном экземпляре</summary>
    ''' <param name="SoleUnitsList">Файлы со списками юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub ReadSoleUnits(ByRef SoleUnitsList() As String)
        If IsNothing(SoleUnitsList) Then Exit Sub
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.SingleUnits}
        For i As Integer = 0 To UBound(SoleUnitsList) Step 1
            s = prepareToFileRead(SoleUnitsList(i), defaultKeys, defaultVals)
            Call ReadFile(6, s, SoleUnitsList(i), AddressOf ReadSoleUnits, defaultKeys)
        Next i
    End Sub
    ''' <summary>Читает список юнитов, которые должны находиться в отряде начиная с заданного количества слотов</summary>
    ''' <param name="BigStackUnitsList">Файлы со списками юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться.
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub ReadBigStackUnits(ByRef BigStackUnitsList() As String)
        If IsNothing(BigStackUnitsList) Then Exit Sub
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.BigStackUnits}
        For i As Integer = 0 To UBound(BigStackUnitsList) Step 1
            s = prepareToFileRead(BigStackUnitsList(i), defaultKeys, defaultVals)
            Call ReadFile(7, s, BigStackUnitsList(i), AddressOf ReadBigStackUnits, defaultKeys)
        Next i
    End Sub
    ''' <summary>Читает список, определяющий принадлежность непроходимых объектов</summary>
    ''' <param name="CustomBuildingRace">Файлы со списками рас и положений зданий. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива (будет прочтен дефолтный).
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub ReadCustomBuildingRace(ByRef CustomBuildingRace() As String)
        If IsNothing(CustomBuildingRace) Then
            Call ReadCustomBuildingRace(New String() {My.Resources.readDefaultFileKeyword})
            Exit Sub
        End If
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.MapObjectRace}
        For i As Integer = 0 To UBound(CustomBuildingRace) Step 1
            s = prepareToFileRead(CustomBuildingRace(i), defaultKeys, defaultVals)
            Call ReadFile(3, s, CustomBuildingRace(i), AddressOf ReadCustomBuildingRace, defaultKeys)
        Next i
    End Sub
    ''' <summary>Читает описание того, как цеплять друг к другу "Плато" и "Водопады"</summary>
    ''' <param name="PlateauConstructionDescription">Файлы с описаниями.
    ''' Допускается передача неинициализитрованного массива (будет прочтен дефолтный).
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    Public Sub ReadPlateauConstructionDescription(ByRef PlateauConstructionDescription() As String)
        If IsNothing(PlateauConstructionDescription) Then
            Call ReadPlateauConstructionDescription(New String() {My.Resources.readDefaultFileKeyword})
            Exit Sub
        End If
        Dim s() As String
        Dim defaultKeys() As String = New String() {My.Resources.readDefaultFileKeyword}
        Dim defaultVals() As String = New String() {My.Resources.PlateauConstructor}
        For i As Integer = 0 To UBound(PlateauConstructionDescription) Step 1
            s = prepareToFileRead(PlateauConstructionDescription(i), defaultKeys, defaultVals)
            Call ReadFile(4, s, PlateauConstructionDescription(i), AddressOf ReadPlateauConstructionDescription, defaultKeys)
        Next i
    End Sub
    Private Function prepareToFileRead(ByRef filePath As String, ByRef defaultKeywords() As String, ByRef defaultValues() As String) As String()
        If Not IsNothing(defaultKeywords) Then
            For i As Integer = 0 To UBound(defaultKeywords) Step 1
                If filePath.ToLower = defaultKeywords(i).ToLower Then Return TxtSplit(defaultValues(i))
            Next i
        End If
        If IO.File.Exists(filePath) Then
            Return TxtSplit(IO.File.ReadAllText(filePath))
        Else
            Return Nothing
        End If
    End Function
    Private Sub ReadFile(ByRef mode As Integer, ByRef s() As String, ByRef filepath As String, _
                         ByRef f As readFunction, ByRef defaultKeys() As String)
        If IsNothing(s) Then Exit Sub
        Dim srow(), r As String
        Dim isKey As Boolean
        For j As Integer = 0 To UBound(s) Step 1
            srow = s(j).Split(CChar(" "))
            isKey = False
            For Each key As String In defaultKeys
                If srow(0).ToLower = key.ToLower And Not filepath.ToLower = key.ToLower Then
                    Call f(New String() {key.ToLower})
                    isKey = True
                    Exit For
                End If
            Next key
            If Not isKey Then
                If mode = 1 Then
                    If Not excludedObjects.Contains(srow(0).ToUpper) Then excludedObjects.Add(srow(0).ToUpper)
                ElseIf mode = 2 Then
                    If srow.Length > 2 Then
                        If customRace.ContainsKey(srow(0).ToUpper) Then customRace.Remove(srow(0).ToUpper)
                        customRace.Add(srow(0).ToUpper, srow(2).ToUpper)
                    End If
                ElseIf mode = 3 Then
                    If srow.Length > 1 Then
                        If objectRace.ContainsKey(srow(0).ToUpper) Then objectRace.Remove(srow(0).ToUpper)
                        objectRace.Add(srow(0).ToUpper, New DecorationPlacingProperties(srow, Me))
                    End If
                ElseIf mode = 4 Then
                    If srow.Length > 1 Then
                        If PlateauConstruction.ContainsKey(srow(0).ToUpper) Then PlateauConstruction.Remove(srow(0).ToUpper)
                        r = ""
                        For i As Integer = 1 To UBound(srow) Step 1
                            If i > 1 Then r &= " "
                            r &= srow(i)
                        Next i
                        PlateauConstruction.Add(srow(0).ToUpper, r.ToUpper)
                    End If
                ElseIf mode = 5 Then
                    If srow.Length > 1 Then
                        If LootItemChanceMultiplier.ContainsKey(srow(0).ToUpper) Then LootItemChanceMultiplier.Remove(srow(0).ToUpper)
                        LootItemChanceMultiplier.Add(srow(0).ToUpper, ValueConverter.StrToDbl(srow(1)))
                    End If
                ElseIf mode = 6 Then
                    For i As Integer = 0 To UBound(srow) Step 1
                        If SoleUnits.ContainsKey(srow(i).ToUpper) Then SoleUnits.Remove(srow(i).ToUpper)
                        SoleUnits.Add(srow(i).ToUpper, New List(Of String))
                        For k As Integer = 0 To UBound(srow) Step 1
                            SoleUnits.Item(srow(i).ToUpper).Add(srow(k).ToUpper)
                        Next k
                    Next i
                ElseIf mode = 7 Then
                    If srow.Length > 1 Then
                        If BigStackUnits.ContainsKey(srow(0).ToUpper) Then BigStackUnits.Remove(srow(0).ToUpper)
                        BigStackUnits.Add(srow(0).ToUpper, CInt(srow(1)))
                    End If
                Else
                    Throw New Exception("Invalid read mode: " & mode)
                End If
            End If
        Next j
    End Sub

    ''' <summary>Вернет нижнюю границу сучайного числа</summary>
    ''' <param name="ratio">Отношение значения верхней границы к значению нижней</param>
    ''' <param name="average">Среднее значение</param>
    Friend Shared Function ValueLowerBound(ByRef ratio As Double, ByRef average As Double) As Double
        If ratio < 1 Then Throw New Exception("Invalid ratio between max and min value: " & ratio & "(<1)")
        'max=ratio*min
        'max+min=2*average
        Return 2 * average / (ratio + 1)
    End Function
    ''' <summary>Вернет верхнюю границу сучайного числа</summary>
    ''' <param name="ratio">Отношение значения верхней границы к значению нижней</param>
    ''' <param name="average">Среднее значение</param>
    Friend Shared Function ValueUpperBound(ByRef ratio As Double, ByRef average As Double) As Double
        Return Common.ValueLowerBound(ratio, average) * ratio
    End Function

    Friend Function ItemTypeCostModify(ByRef item As AllDataStructues.Item) As AllDataStructues.Cost
        If itemType.Item(item.type) = "JEWEL" Then
            Return item.itemCost / valConv.JewelItemsCostDevider
        Else
            Return item.itemCost / valConv.nonJewelItemsCostDevider
        End If
    End Function
End Class

Public MustInherit Class DecorationPlacingPropertiesFields
    ''' <summary>Допустимые расы</summary>
    Public race As New List(Of Integer)
    ''' <summary>Можно лит ставить на суше</summary>
    Public ground As Boolean
    ''' <summary>Можно лит ставить на воде</summary>
    Public water As Boolean
End Class

Public Class DecorationPlacingProperties
    Inherits DecorationPlacingPropertiesFields

    Public Sub New()
    End Sub
    ''' <param name="racesRow">Первый столбец - идентификаторы объектов. В остальных - допустимые места расстановки объектов и расы</param>
    ''' <param name="comm">Инициализированный класс</param>
    Public Sub New(ByRef racesRow() As String, ByRef comm As Common)
        Call applyRaceRow(racesRow, comm)
    End Sub

    Private Sub applyRaceRow(ByRef racesRow() As String, ByRef comm As Common)
        Dim id As Integer
        For i As Integer = 1 To UBound(racesRow) Step 1
            If racesRow(i).ToUpper = "G" Then
                ground = True
            ElseIf racesRow(i).ToUpper = "W" Then
                water = True
            Else
                Try
                    id = comm.RaceIdentifierToSubrace(racesRow(i))
                    If Not race.Contains(id) Then race.Add(id)
                Catch ex As Exception
                    Console.WriteLine(ex.Message)
                End Try
            End If
        Next i
    End Sub

End Class

Public Class AllDataStructues

    Public Structure DesiredStats
        ''' <summary>Примерная планка опыта для маленьких воинов</summary>
        Dim ExpBarAverage As Integer
        ''' <summary>Допустимые расы для отряда</summary>
        Dim Race As List(Of Integer)
        ''' <summary>Примерный опыт за убийство стэка</summary>
        Dim ExpStackKilled As Integer
        ''' <summary>Количество свободных ячеек под отряд. Есть 10% шанс на +1 слот и 10% неа -1 слот</summary>
        Dim StackSize As Integer
        ''' <summary>Максимальное количество больших воинов в отряде</summary>
        Dim MaxGiants As Integer
        ''' <summary>Сколько ячеек в первом ряду должно быть заполнено</summary>
        Dim MeleeCount As Integer
        ''' <summary>Стоимость лута (предметы со стоимостью в золоте, равной нулю, не добавляются). При расчете стоимость драгоценностей уменьшается в два раза</summary>
        Dim LootCost As Integer
        ''' <summary>Идентификатор локации, для которой сгенерирован отряд</summary>
        Dim LocationName As String
        ''' <summary>Настройки генерации предметов</summary>
        Dim IGen As LootGenSettings

        ''' <summary>Не nothing только для торговцев предметами и магией, а также лагеря наемников.
        ''' Список идентификаторов содержимого лавки с предметами/заклинаниями/наемниками, 
        ''' либо параметра генерации (цена, тип или тип#цена для предмета, уровень для заклинания и 
        ''' планка опыта на 1 занимаемый слот для существа).
        ''' Типы предметов можно посмотреть в Items.txt или Common.itemType
        ''' Примеры:
        ''' Наемник - g000uu0001 200 700
        ''' Волшебник - g000ss0005 g000ss0006 1RT 2CF (T - может быть глобальным, F - не может. H,L,C,U,E - раса, R - случайная раса)
        ''' Торговец - g000ig0002 1200 750 attack_artefact sphere#300 6#400
        ''' </summary>
        Dim shopContent As List(Of String)

        ''' <summary>True, если отряд является внутренней охраной города</summary>
        Dim isInternalCityGuard As Boolean

        Public Shared Function Copy(ByVal v As DesiredStats) As DesiredStats
            Dim RacesList As List(Of Integer) = Nothing
            If Not IsNothing(v.Race) Then
                RacesList = New List(Of Integer)
                For Each Item As Integer In v.Race
                    RacesList.Add(Item)
                Next Item
            End If
            Dim shopContentList As List(Of String) = Nothing
            If Not IsNothing(v.shopContent) Then
                shopContentList = New List(Of String)
                For Each Item As String In v.shopContent
                    shopContentList.Add(Item)
                Next Item
            End If
            Return New DesiredStats With {.ExpBarAverage = v.ExpBarAverage, _
                                          .ExpStackKilled = v.ExpStackKilled, _
                                          .MaxGiants = v.MaxGiants, _
                                          .MeleeCount = v.MeleeCount, _
                                          .Race = RacesList, _
                                          .StackSize = v.StackSize, _
                                          .LootCost = v.LootCost, _
                                          .LocationName = v.LocationName, _
                                          .IGen = AllDataStructues.LootGenSettings.copy(v.IGen), _
                                          .shopContent = shopContentList, _
                                          .isInternalCityGuard = v.isInternalCityGuard}
        End Function
        ''' <param name="RaceNumberToRaceChar">Преобразует номер расы в ее текстовый идентификатор. Если передать Nothing, то будут печататься номера рас</param>
        Public Shared Function Print(ByVal v As DesiredStats, ByRef RaceNumberToRaceChar As Dictionary(Of Integer, String), Optional ByVal shortOut As Boolean = False) As String
            Dim s As String
            If IsNothing(v.shopContent) Then
                Dim races As String = ""
                If Not shortOut Then
                    For Each Item As Integer In v.Race
                        If Not races = "" Then races &= "+"
                        If Not IsNothing(RaceNumberToRaceChar) Then
                            races &= RaceNumberToRaceChar.Item(Item)
                        Else
                            races &= Item
                        End If
                    Next Item
                End If
                s = "AverageExpBar" & vbTab & v.ExpBarAverage & vbNewLine & _
                    "ExpStackKilled" & vbTab & v.ExpStackKilled & vbNewLine

                If Not shortOut Then
                    s &= "Race" & vbTab & races & vbNewLine
                End If
                s &= "StackSize" & vbTab & v.StackSize & vbNewLine & _
                     "MaxGiants" & vbTab & v.MaxGiants & vbNewLine & _
                     "MeleeCount" & vbTab & v.MeleeCount & vbNewLine
                If Not shortOut Then
                    s &= "LootCost" & vbTab & v.LootCost & vbNewLine & _
                         "IsInternalCityGuard" & vbTab & v.isInternalCityGuard & vbNewLine & _
                         AllDataStructues.LootGenSettings.print(v.IGen) & vbNewLine
                    s = "ID" & vbTab & v.LocationName & vbNewLine & s
                End If
            Else
                Dim goods As String = ""
                For Each Item As String In v.shopContent
                    If Not goods = "" Then goods &= "+"
                    goods &= Item
                Next Item
                s = "ShopContent" & vbTab & goods & vbNewLine
                s = "ID" & vbTab & v.LocationName & vbNewLine & s
            End If
            Return s
        End Function
    End Structure

    Public Structure Stack
        ''' <summary>ID юнита для каждой позиции</summary>
        Dim pos() As String
        ''' <summary>Уровень юнита для каждой позиции</summary>
        Dim level() As Integer
        ''' <summary>В какой позиции находится лидер</summary>
        Dim leaderPos As Integer
        ''' <summary>Предметы отряда. GxxxIGxxxx</summary>
        Dim items As List(Of String)
        ''' <summary>Имя отряда</summary>
        Dim name As String
    End Structure

    Public Structure Unit
        ''' <summary>Имя</summary>
        Dim name As String
        ''' <summary>Базовый уровень</summary>
        Dim level As Integer
        ''' <summary>Номер расы</summary>
        Dim race As Integer
        ''' <summary>Опыт за убийство юнита</summary>
        Dim EXPkilled As Integer
        ''' <summary>Опыт для апа уровня</summary>
        Dim EXPnext As Integer
        ''' <summary>Лидерство от 0 до 6</summary>
        Dim leadership As Integer
        ''' <summary>Область атаки. 1 – все цели; 2 – любая цель; 3 – ближайшая цель.</summary>
        Dim reach As Integer
        ''' <summary>GxxxUUxxxx</summary>
        Dim unitID As String
        ''' <summary>True, если занимает одну клетку</summary>
        Dim small As Boolean
        ''' <summary>True, если может находиться только на воде</summary>
        Dim waterOnly As Boolean
        ''' <summary>0 - мили, 1 - лучники, 2 - маги, 3 - поддержка, 4 - особые (оборотень, сатир и т.д.), 
        ''' 5 - обычный лидер, 6 - вор, 7 - саммон</summary>
        Dim unitBranch As Integer
        ''' <summary>Цена найма юнита</summary>
        Dim unitCost As Cost
        ''' <summary>Уровень, до которого статы растут согласно dynUpgr1</summary>
        Dim dynUpgradeLevel As Integer
        ''' <summary>Рост статов до dynUpgradeLevel</summary>
        Dim dynUpgrade1 As DynUpgrade
        ''' <summary>Рост статов после dynUpgradeLevel</summary>
        Dim dynUpgrade2 As DynUpgrade

        Public Shared Function Copy(ByVal v As Unit) As Unit
            Return New Unit With {.name = v.name, _
                                  .level = v.level, _
                                  .race = v.race, _
                                  .EXPkilled = v.EXPkilled, _
                                  .EXPnext = v.EXPnext, _
                                  .leadership = v.leadership, _
                                  .reach = v.reach, _
                                  .unitID = v.unitID.ToUpper, _
                                  .small = v.small, _
                                  .waterOnly = v.waterOnly, _
                                  .unitBranch = v.unitBranch, _
                                  .unitCost = Cost.Copy(v.unitCost), _
                                  .dynUpgradeLevel = v.dynUpgradeLevel, _
                                  .dynUpgrade1 = DynUpgrade.Copy(v.dynUpgrade1), _
                                  .dynUpgrade2 = DynUpgrade.Copy(v.dynUpgrade2)}
        End Function
    End Structure

    Public Structure DynUpgrade
        ''' <summary>Опыт за убийство юнита</summary>
        Dim EXPkilled As Integer
        ''' <summary>Опыт для апа уровня</summary>
        Dim EXPnext As Integer
        ''' <summary>Цена найма юнита</summary>
        Dim unitCost As Cost

        Public Shared Function Copy(ByVal v As DynUpgrade) As DynUpgrade
            Return New DynUpgrade With {.EXPkilled = v.EXPkilled, _
                                        .EXPnext = v.EXPnext, _
                                        .unitCost = Cost.Copy(v.unitCost)}
        End Function
    End Structure

    Public Structure Cost
        ''' <summary>Золото</summary>
        Dim Gold As Integer
        ''' <summary>Мана Жизни</summary>
        Dim Blue As Integer
        ''' <summary>Мана Преисподней</summary>
        Dim Red As Integer
        ''' <summary>Мана Рун</summary>
        Dim White As Integer
        ''' <summary>Мана Смерти</summary>
        Dim Black As Integer
        ''' <summary>Мана Лесного Эликсира</summary>
        Dim Green As Integer

        Public Shared Function Copy(ByVal v As Cost) As Cost
            Return New Cost With {.gold = v.Gold, _
                                  .Blue = v.Blue, _
                                  .Red = v.Red, _
                                  .White = v.White, _
                                  .Black = v.Black, _
                                  .Green = v.Green}
        End Function
        ''' <summary>Парсит строку стоимости в родном для D2 формате. Игнорирует регистр, пробелы и табы. Пропущенные поля интерпретирует как ноль</summary>
        ''' <param name="costString">g0000:r0000:y0000:e0000:w0000:b0000</param>
        Public Shared Function Read(ByVal costString As String) As Cost
            Dim splited() As String = costString.Replace(" ", "").Replace(vbTab, "").ToLower.Split(CChar(":"))
            Dim res As New Cost
            For i As Integer = 0 To UBound(splited) Step 1
                Dim s1 As String = splited(i).Substring(0, 1)
                Dim v As String = splited(i).Substring(1)
                Do While v.Substring(0, 1) = "0" And v.Length > 1
                    v = v.Substring(1)
                Loop
                If Not IsNumeric(v) Then
                    Throw New Exception("Количество ресурса не является числом: " & costString & " , ресурс: " & s1)
                    Return Nothing
                End If
                If s1 = "g" Then
                    res.Gold = ValueConverter.StrToInt(v, costString, s1)
                ElseIf s1 = "r" Then
                    res.Red = ValueConverter.StrToInt(v, costString, s1)
                ElseIf s1 = "y" Then
                    res.Blue = ValueConverter.StrToInt(v, costString, s1)
                ElseIf s1 = "e" Then
                    res.Black = ValueConverter.StrToInt(v, costString, s1)
                ElseIf s1 = "w" Then
                    res.White = ValueConverter.StrToInt(v, costString, s1)
                ElseIf s1 = "b" Then
                    res.Green = ValueConverter.StrToInt(v, costString, s1)
                Else
                    Throw New Exception("Неожиданный формат стоимости: " & costString)
                    Return Nothing
                End If
            Next i
            Return res
        End Function
        ''' <summary>Печатает стоимость в понятном для игры формате</summary>
        ''' <param name="v">цена: золото и мана</param>
        Public Shared Function Print(ByVal v As Cost) As String
            Dim ch() As String = New String() {"g", "r", "y", "e", "w", "b"}
            Dim val() As Integer = New Integer() {v.Gold, v.Red, v.Blue, v.Black, v.White, v.Green}
            Dim s As String = ""
            For i As Integer = 0 To UBound(ch) Step 1
                s &= ch(i)
                If val(i) > 9999 Then
                    Throw New Exception("Too great value of " & ch(i) & " : " & val(i))
                ElseIf val(i) < 1000 Then
                    If val(i) > 99 Then
                        s &= "0"
                    ElseIf val(i) > 9 Then
                        s &= "00"
                    Else
                        s &= "000"
                    End If
                End If
                s &= val(i).ToString
                If i < UBound(ch) Then s &= ":"
            Next i
            Return s
        End Function

        ''' <summary>Вернет суммарную стоимость в золоте и мане</summary>
        ''' <param name="v">цена: золото и мана</param>
        Public Shared Function Sum(ByVal v As Cost) As Integer
            Return v.Gold + v.Black + v.Blue + v.Green + v.Red + v.White
        End Function

        Public Shared Operator *(ByVal v As Cost, ByVal n As Double) As Cost
            Return New Cost With {.Black = CInt(v.Black * n), _
                                  .Blue = CInt(v.Blue * n), _
                                  .Gold = CInt(v.Gold * n), _
                                  .Green = CInt(v.Green * n), _
                                  .Red = CInt(v.Red * n), _
                                  .White = CInt(v.White * n)}
        End Operator
        Public Shared Operator *(ByVal n As Double, ByVal v As Cost) As Cost
            Return v * n
        End Operator
        Public Shared Operator /(ByVal v As Cost, ByVal n As Double) As Cost
            Return New Cost With {.Black = CInt(v.Black / n), _
                                  .Blue = CInt(v.Blue / n), _
                                  .Gold = CInt(v.Gold / n), _
                                  .Green = CInt(v.Green / n), _
                                  .Red = CInt(v.Red / n), _
                                  .White = CInt(v.White / n)}
        End Operator
        Public Shared Operator +(ByVal v1 As Cost, ByVal v2 As Cost) As Cost
            Return New Cost With {.Black = v1.Black + v2.Black, _
                                  .Blue = v1.Blue + v2.Blue, _
                                  .Gold = v1.Gold + v2.Gold, _
                                  .Green = v1.Green + v2.Green, _
                                  .Red = v1.Red + v2.Red, _
                                  .White = v1.White + v2.White}
        End Operator
        Public Shared Operator -(ByVal v1 As Cost, ByVal v2 As Cost) As Cost
            Return New Cost With {.Black = v1.Black - v2.Black, _
                                  .Blue = v1.Blue - v2.Blue, _
                                  .Gold = v1.Gold - v2.Gold, _
                                  .Green = v1.Green - v2.Green, _
                                  .Red = v1.Red - v2.Red, _
                                  .White = v1.White - v2.White}
        End Operator

    End Structure

    Public Structure Item
        ''' <summary>Название</summary>
        Dim name As String
        ''' <summary>GxxxIGxxxx</summary>
        Dim itemID As String
        ''' <summary>Описание типов в ./Resources/Items.txt</summary>
        Dim type As Integer
        ''' <summary>Цена покупки предмета. При продаже цена в пять раз меньше</summary>
        Dim itemCost As Cost

        Public Shared Function Copy(ByVal v As Item) As Item
            Return New Item With {.name = v.name, _
                                  .itemID = v.itemID, _
                                  .type = v.type, _
                                  .itemCost = Cost.Copy(v.itemCost)}
        End Function
    End Structure

    Public Structure StackStatsField
        Dim name As String
        Dim description As String
    End Structure

    Public Structure Spell
        ''' <summary>ID заклинания</summary>
        Dim spellID As String
        ''' <summary>Название заклинания</summary>
        Dim name As String
        ''' <summary>Цена изучения для каждого лорда. Ключ - id лорда в верхнем регистре. Список лордов хранится в Common.LordsRace</summary>
        Dim researchCost As Dictionary(Of String, AllDataStructues.Cost)
        ''' <summary>Цена применения</summary>
        Dim castCost As AllDataStructues.Cost
        ''' <summary>Уровень заклинания</summary>
        Dim level As Integer
        ''' <summary>Тип заклинания</summary>
        Dim category As Integer
        ''' <summary>Площадь действия заклинания</summary>
        Dim area As Integer
    End Structure

    Public Structure LootGenSettings
        ''' <summary>Cферы, талисманы и свитки</summary>
        Dim ConsumableItems As ItemGenSettings
        ''' <summary>Надеваемые предметы и посохи</summary>
        Dim NonconsumableItems As ItemGenSettings
        ''' <summary>Драгоценности</summary>
        Dim JewelItems As ItemGenSettings

        Public Shared Function Copy(ByVal v As LootGenSettings) As LootGenSettings
            Return New LootGenSettings With {.ConsumableItems = AllDataStructues.ItemGenSettings.Copy(v.ConsumableItems), _
                                             .NonconsumableItems = AllDataStructues.ItemGenSettings.Copy(v.NonconsumableItems), _
                                             .JewelItems = AllDataStructues.ItemGenSettings.Copy(v.JewelItems)}
        End Function
        Public Shared Function Print(ByVal v As LootGenSettings) As String
            Return "CItemsGen" & vbTab & AllDataStructues.ItemGenSettings.Print(v.ConsumableItems) & vbNewLine & _
                   "NItemsGen" & vbTab & AllDataStructues.ItemGenSettings.Print(v.NonconsumableItems) & vbNewLine & _
                   "JItemsGen" & vbTab & AllDataStructues.ItemGenSettings.Print(v.JewelItems)
        End Function
        ''' <summary>Return {ConsumableItems,NonconsumableItems,JewelItems}</summary>
        Public Shared Function ToArray(ByVal v As LootGenSettings) As ItemGenSettings()
            Return New ItemGenSettings() {v.ConsumableItems, v.NonconsumableItems, v.JewelItems}
        End Function
    End Structure

    Public Structure ItemGenSettings
        ''' <summary>Не генерировать</summary>
        Dim exclude As Boolean
        ''' <summary>Примерное количество. Игнорируется, если меньше 1</summary>
        Dim amount As Integer
        ''' <summary>Доля от общей стоимости лута. Игнорируется, если равно 0</summary>
        Dim costPart As Double
        ''' <summary>costPart*TotalLootCost</summary>
        Friend dynCostPart As Integer

        Public Shared Function Copy(ByVal v As ItemGenSettings) As ItemGenSettings
            Return New ItemGenSettings With {.exclude = v.exclude, _
                                             .amount = v.amount, _
                                             .costPart = v.costPart}
        End Function

        Public Shared Function Print(ByVal v As ItemGenSettings) As String
            Return v.exclude.ToString & "#" & v.amount.ToString & "#" & v.costPart.ToString
        End Function
        Public Shared Function Read(ByVal v As String) As ItemGenSettings
            Dim s() As String = v.Replace(" ", "").Replace(vbTab, "").Split(CChar("#"))
            Dim n As Integer = 3
            If Not s.Length = n Then Throw New Exception("ItemGenSettings.Read expects " & n & " values")
            Return New ItemGenSettings With {.exclude = ValueConverter.StrToBool(s(0)), _
                                             .amount = ValueConverter.StrToInt(s(1), v, "second"), _
                                             .costPart = ValueConverter.StrToDbl(s(2))}
        End Function
    End Structure

End Class

Friend Class ValueConverter

    Private WeightMultiplicatorReplaced As String = ""

    Friend Shared Function StrToDbl(ByRef s As String) As Double
        Return CDbl(s.Replace(",", ".").Replace(".", System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator))
        'Return Convert.ToDouble(s, Globalization.NumberFormatInfo.InvariantInfo)
    End Function

    Friend Shared Function StrToInt(ByRef v As String, ByRef fullLine As String, ByRef fieldName As String) As Integer
        Try
            Return CInt(v)
        Catch ex As Exception
            Throw New Exception(ex.Message & vbNewLine & fullLine & vbNewLine & "Field: " & fieldName)
            Return 1
        End Try
    End Function

    Friend Shared Function StrToBool(ByRef v As String) As Boolean
        Dim f As String = v.ToUpper
        If f = "T" Or f = "TRUE" Or f = "1" Then
            Return True
        Else
            Return False
        End If
    End Function


    Friend Function defaultSigma() As Double
        Return 0.1
    End Function

    'units
    Friend Function expBarDispersion() As Double
        Return 1.3
    End Function
    Friend Function giantUnitsExpMultiplicator() As Double
        Return 2
    End Function
    Friend Function smallUnitsExpMultiplicator() As Double
        Return 1
    End Function

    'loot
    Friend Function WeightMultiplicator() As String
        If WeightMultiplicatorReplaced = "" Then
            WeightMultiplicatorReplaced = "talisman=0.1;" & _
                                          "scroll=0.333;" & _
                                          "stuff=0.75;" & _
                                          "healing_elixir=3#cost>100else1;" & _
                                          "ressurection_elixir=3;" & _
                                          "permanent_elixir=0.6;" & _
                                          "elixir=0.75#cost>400else1.15"
            WeightMultiplicatorReplaced = WeightMultiplicatorReplaced.Replace(",", ".").Replace(".", System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator)
        End If
        Return WeightMultiplicatorReplaced
    End Function
    Friend Function JewelItemsCostDevider() As Double
        Return 2
    End Function
    Friend Function nonJewelItemsCostDevider() As Double
        Return 1
    End Function
    Friend Function lootCostDispersion() As Double
        Return 2
    End Function

    'map
    Friend Function minLocationRadiusAtAll() As Double
        Return 7
    End Function

End Class

Public Class Log

    Private Enabled As Boolean
    Private Content As New List(Of String)

    Private multiThreadLog() As List(Of String)

    Private comm As Common

    Delegate Function printWithNoInput() As String
    Delegate Function printSelectionList(ByRef units() As AllDataStructues.Unit, ByRef possible As List(Of Integer)) As String

    Public Sub New(ByRef c As Common)
        If IsNothing(c) Then Throw New Exception("В класс Log нужно передавать инициализированный класс Common")
        comm = c
    End Sub

    ''' <summary>Включить логирование</summary>
    Public Sub Enable()
        Enabled = True
    End Sub
    ''' <summary>Выключить логирование</summary>
    Public Sub Disable()
        Enabled = False
    End Sub
    ''' <summary>Узнать, включено ли логирование</summary>
    Public Function IsEnabled() As Boolean
        Return Enabled
    End Function

    ''' <summary>Очистить лог</summary>
    Public Sub Clear()
        Content.Clear()
    End Sub
    ''' <summary>Вернет количество записей в логе</summary>
    Public Function Size() As Integer
        Return Content.Count
    End Function
    ''' <summary>Вернет запись с указанным номером (от 0 до Size-1)</summary>
    Public Function PrintItem(ByVal id As Integer) As String
        Return LogPrint(Content, id)
    End Function
    ''' <summary>Вернет все записи</summary>
    Public Function PrintAll() As String
        Return LogPrint(Content)
    End Function

    Private Function LogPrint(ByRef log As List(Of String), ByRef id As Integer) As String
        Return log.Item(id)
    End Function
    Private Function LogPrint(ByRef log As List(Of String)) As String
        Dim result As String = ""
        Dim boofer As String = ""
        Dim len As Integer = log.Count - 1
        If len = -1 Then Return result
        result = LogPrint(log, 0)
        For i As Integer = 1 To len Step 1
            boofer &= vbNewLine & LogPrint(log, i)
            If boofer.Length > 10000 Then
                result &= boofer
                boofer = ""
            End If
        Next i
        If Not boofer = "" Then result &= boofer
        Return result
    End Function

    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="contString">Строка с записью</param>
    Public Sub Add(ByVal contString As String)
        If Not Enabled Then Exit Sub
        Content.Add(contString)
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="contString">Будет добавлен результат выполнения этой функции</param>
    Public Sub Add(ByRef contString As printWithNoInput)
        If Not Enabled Then Exit Sub
        Content.Add(contString())
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="contString">Будут добавлены поля этой переменной</param>
    Public Sub Add(ByRef contString As AllDataStructues.DesiredStats, ByVal shortOut As Boolean)
        If Not Enabled Then Exit Sub
        Content.Add(AllDataStructues.DesiredStats.Print(contString, comm.RaceNumberToRaceChar, shortOut))
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="contString">Будут добавлены поля этой переменной</param>
    Public Sub Add(ByRef contString As AllDataStructues.Cost)
        If Not Enabled Then Exit Sub
        Content.Add(AllDataStructues.Cost.Print(contString))
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="contString">Будут добавлены поля этой переменной</param>
    Public Sub Add(ByRef contString As AllDataStructues.LootGenSettings)
        If Not Enabled Then Exit Sub
        Content.Add(AllDataStructues.LootGenSettings.Print(contString))
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="contString">Будет добавлен результат выполнения этой функции</param>
    Public Sub Add(ByRef contString As printSelectionList, ByVal v() As AllDataStructues.Unit, ByRef i As List(Of Integer))
        If Not Enabled Then Exit Sub
        Content.Add(contString(v, i))
    End Sub

    ''' <summary>Очистить лог с заданным номером</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    Public Sub MClear(ByVal LogID As Integer)
        multiThreadLog(LogID).Clear()
    End Sub
    ''' <summary>Очистить все мультизадачные логи</summary>
    Public Sub MClear()
        If IsNothing(multiThreadLog) Then Exit Sub
        For LogID As Integer = 0 To UBound(multiThreadLog) Step 1
            MClear(LogID)
        Next LogID
    End Sub
    ''' <summary>Вернет количество записей в логе</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    Public Function MSize(ByVal LogID As Integer) As Integer
        Return multiThreadLog(LogID).Count
    End Function
    ''' <summary>Вернет количество мультизадачных логов</summary>
    Public Function MSize() As Integer
        If IsNothing(multiThreadLog) Then
            Return 0
        Else
            Return multiThreadLog.Length
        End If
    End Function
    ''' <summary>Вернет запись с указанным номером (от 0 до Size-1)</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    Public Function MPrintItem(ByVal LogID As Integer, ByVal id As Integer) As String
        Return LogPrint(multiThreadLog(LogID), id)
    End Function
    ''' <summary>Вернет все записи</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    Public Function MPrintAll(ByVal LogID As Integer) As String
        Return LogPrint(multiThreadLog(LogID))
    End Function
    ''' <summary>Вернет все записи в мультизадачных логах</summary>
    Public Function MPrintAll() As String
        Dim result As String = ""
        Dim len As Integer = MSize() - 1
        If len = -1 Then Return result
        result = MPrintAll(0)
        For i As Integer = 1 To len Step 1
            result &= vbNewLine & MPrintAll(i)
        Next i
        Return result
    End Function

    ''' <summary>Создает логи под несколько задач, если логирование включено</summary>
    ''' <param name="size">Количество задач, больше 0. Если меньше 1 - уничтожит массив</param>
    Public Sub MRedim(ByVal size As Integer)
        If Not Enabled Then Exit Sub
        If size > 0 Then
            ReDim multiThreadLog(size - 1)
            For i As Integer = 0 To size - 1 Step 1
                multiThreadLog(i) = New List(Of String)
            Next i
        Else
            Call MClear()
            multiThreadLog = Nothing
        End If
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    ''' <param name="contString">Строка с записью</param>
    Public Sub MAdd(ByVal LogID As Integer, ByVal contString As String)
        If Not Enabled Then Exit Sub
        multiThreadLog(LogID).Add(contString)
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    ''' <param name="contString">Будет добавлен результат выполнения этой функции</param>
    Public Sub MAdd(ByVal LogID As Integer, ByRef contString As printWithNoInput)
        If Not Enabled Then Exit Sub
        multiThreadLog(LogID).Add(contString())
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    ''' <param name="contString">Будут добавлены поля этой переменной</param>
    Public Sub MAdd(ByVal LogID As Integer, ByVal contString As AllDataStructues.DesiredStats, ByVal shortOut As Boolean)
        If Not Enabled Then Exit Sub
        multiThreadLog(LogID).Add(AllDataStructues.DesiredStats.Print(contString, comm.RaceNumberToRaceChar, shortOut))
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    ''' <param name="contString">Будут добавлены поля этой переменной</param>
    Public Sub MAdd(ByVal LogID As Integer, ByVal contString As AllDataStructues.Cost)
        If Not Enabled Then Exit Sub
        multiThreadLog(LogID).Add(AllDataStructues.Cost.Print(contString))
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    ''' <param name="contString">Будут добавлены поля этой переменной</param>
    Public Sub MAdd(ByVal LogID As Integer, ByRef contString As AllDataStructues.LootGenSettings)
        If Not Enabled Then Exit Sub
        Content.Add(AllDataStructues.LootGenSettings.Print(contString))
    End Sub
    ''' <summary>Добавить запись в лог, если логирование включено</summary>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1</param>
    ''' <param name="contString">Будет добавлен результат выполнения этой функции</param>
    Public Sub MAdd(ByVal LogID As Integer, ByRef contString As printSelectionList, ByVal v() As AllDataStructues.Unit, ByRef i As List(Of Integer))
        If Not Enabled Then Exit Sub
        multiThreadLog(LogID).Add(contString(v, i))
    End Sub

End Class