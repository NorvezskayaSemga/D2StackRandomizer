Imports System.Threading.Tasks

Public Class RandStack

    Public Class ConstructorInput
        ''' <summary>
        ''' Dсе юниты в игре
        ''' </summary>
        Public AllUnitsList() As AllDataStructues.Unit
        ''' <summary>
        ''' >Все предметы в игре
        ''' </summary>
        Public AllItemsList() As AllDataStructues.Item
        ''' <summary>
        ''' Все заклинания в игре
        ''' </summary>
        Public AllSpellsList() As AllDataStructues.Spell
        ''' <summary>
        ''' Все модификаторы в игре
        ''' </summary>
        Public AllModificatorsList() As AllDataStructues.Modificator
        ''' <summary>
        ''' Настройки генерации
        ''' </summary>
        Public settings As New SettingsInfo
        ''' <summary>
        ''' Информация о карте
        ''' </summary>
        Public mapData As New MapInfo
        ''' <summary>
        ''' Кастомная информация о моде
        ''' </summary>
        Public modData As New CustomModSettings

        Public Class MapInfo
            ''' <summary>Положение столиц (угол с наименьшей координатой по X и Y)</summary>
            Public capitalPos() As Point = Nothing
            '''<summary>Список лордов на карте. Юниты из веток развития соответствующих рас добавляться в отряды не будут, 
            ''' если SettingsInfo.AddUnitsFromBranchesToStacks = False и MapLords != Nothing.</summary>
            Public mapLords() As String = Nothing
            ''' <summary>
            ''' Количество рудников на карте. По умолчанию всех по одному
            ''' </summary>
            Public minesAmount As New AllDataStructues.Cost With {.Black = 1, _
                                                                  .Blue = 1, _
                                                                  .Gold = 1, _
                                                                  .Green = 1, _
                                                                  .Red = 1, _
                                                                  .White = 1}
        End Class
        Public Class SettingsInfo
            ''' <summary>
            ''' Название мода, на котором происходит генерация.
            ''' Ваианты можно получить из GenDefaultValues.GetSupportedMods
            ''' </summary>
            Public modName As String

            ''' <summary>
            ''' Количество использований талисманов. По умолчанию пять
            ''' </summary>
            Public talismanChargesDefaultAmount As Integer = 5

            ''' <summary>Если True, генератор будет игнорировать ограничения по расе при создании отрядов. По умолчанию False</summary>
            Public ignoreUnitRace As Boolean
            ''' <summary>Если True, генератор не будет добавлять в отряды юнитов, связнных с лором. По умолчанию False</summary>
            Public excludeLoreUnits As Boolean
            ''' <summary>Если True, генератор будет сохранять в отрядах юнитов, связнных с лором. По умолчанию False</summary>
            Public preserveLoreUnits As Boolean
            ''' <summary>Если False, генератор не будет добавлять в отряды юнитов из веток развития, если столица их расы есть на карте. По умолчанию False</summary>
            Public addUnitsFromBranchesToStacks As Boolean
            ''' <summary>
            ''' Если True, не будет учитывать оверлевелы при расчете опыта за убийство и планок опыта. Вместо этого добавит оверлевелы новому отряду. По умолчанию False
            ''' </summary>
            Public preserveUnitsOverlevel As Boolean

            '''<summary>
            ''' При перегенерации предметов если в луте было знамя, то и после перегенерации будет какое-то знамя. 
            ''' Если были сапоги - будут какие-нибудь сапоги.
            ''' </summary>
            Public ApplyStrictTypesFilter As Boolean
        End Class
        Public Class CustomModSettings
            ''' <summary>
            ''' ID исключенных юнитов, предметов и заклинаний.
            ''' Если Nothing - будут использованы стандартные настройки для мода
            ''' </summary>
            Public ExcludedObjects As List(Of String) = Nothing
            ''' <summary>
            ''' Ключ - идентификатор предмета, Значение - число, на которое дополнительно умножается шанс появления предмета.
            ''' Если Nothing - будут использованы стандартные настройки для мода
            ''' </summary>
            Public LootItemChanceMultiplier As Dictionary(Of String, Double) = Nothing
            ''' <summary>
            ''' ID предметов, которые генератор должен оставлять на месте.
            ''' Если Nothing - будут использованы стандартные настройки для мода
            ''' </summary>
            Public PreservedItems As List(Of String) = Nothing

            Friend Shared Function ToList(ByRef d As Dictionary(Of String, Double)) As List(Of String)
                Dim result As New List(Of String)
                Dim keys As List(Of String) = d.Keys.ToList
                For Each k As String In keys
                    result.Add(k & " " & d.Item(k))
                Next k
                Return result
            End Function
        End Class

    End Class

    Private busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
    Private firstrow() As Integer = New Integer() {0, 2, 4}
    Private secondrow() As Integer = New Integer() {1, 3, 5}
    Private Const itemGenSigma As Double = 0.5
    Private Const multiItemGenSigmaMultiplier As Double = 1.5
    Private ReadOnly maxStackSize As Integer = firstrow.Length + secondrow.Length

    Private UnitsArrayPos As New Dictionary(Of String, Integer)
    Friend AllUnits() As AllDataStructues.Unit

    Private ItemsArrayPos As New Dictionary(Of String, Integer)
    Friend AllItems() As AllDataStructues.Item

    Private SpellsArrayPos As New Dictionary(Of String, Integer)
    Friend AllSpells() As AllDataStructues.Spell

    Friend AllModificators As New Dictionary(Of String, AllDataStructues.Modificator)

    Public rndgen As RndValueGen
    Public comm As Common

    Private ExpBar(), ExpKilled(), multiplierUnitDesiredStats() As Double

    Friend ItemCostSum() As Double
    Private minItemGoldCost As Integer
    Private bak_multiplierItemsWeight() As Double

    ''' <summary>
    ''' Настройки генерации
    ''' </summary>
    Public settings As ConstructorInput.SettingsInfo
    ''' <summary>
    ''' Информация о карте
    ''' </summary>
    Public mapData As ConstructorInput.MapInfo

    Friend Global_ItemsWeightMultiplier() As Double
    Private Global_AddedItems As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of AllDataStructues.Item)))

    ''' <summary>Сюда генератор пишет лог</summary>
    Public log As Log

    Public Sub New(ByRef data As ConstructorInput)
        comm = New Common(data.settings.modName) With {.onExcludedListChanged = AddressOf ResetExclusions}
        rndgen = comm.rndgen
        log = New Log(comm)
        settings = data.settings
        mapData = data.mapData

        If IsNothing(data.AllUnitsList) Or IsNothing(data.AllItemsList) Then Exit Sub

        If settings.talismanChargesDefaultAmount < 1 Then Throw New Exception("Unexpected TalismanChargesDefaultAmount")

        If settings.excludeLoreUnits Then Call comm.ReadExcludedLoreObjectsList()
        If settings.preserveLoreUnits Then Call comm.ReadLoreUnitsToPreservedObjects()
        If IsNothing(data.modData.ExcludedObjects) Then
            Call comm.ReadExcludedObjectsList()
        Else
            Call comm.ReadExcludedObjectsList(data.modData.ExcludedObjects)
        End If
        If IsNothing(data.modData.PreservedItems) Then
            Call comm.ReadPreservedObjects()
        Else
            Call comm.ReadPreservedObjects(data.modData.PreservedItems)
        End If
        If IsNothing(data.modData.LootItemChanceMultiplier) Then
            Call comm.ReadLootItemChanceMultiplier()
        Else
            Call comm.ReadLootItemChanceMultiplier(ConstructorInput.CustomModSettings.ToList(data.modData.LootItemChanceMultiplier))
        End If
        Call comm.ReadCustomUnitRace()
        Call comm.ReadSoleUnits()
        Call comm.ReadBigStackUnits()

        Dim PlayableSubraces As New List(Of Integer)
        For Each item As String In comm.TxtSplit(comm.defValues.PlayableSubraces)
            PlayableSubraces.Add(ValueConverter.StrToInt(item, "", ""))
        Next item
        Dim unitSubrace As Integer

        ReDim AllUnits(UBound(data.AllUnitsList)), ExpBar(UBound(data.AllUnitsList)), _
              ExpKilled(UBound(data.AllUnitsList)), multiplierUnitDesiredStats(UBound(data.AllUnitsList))
        For i As Integer = 0 To UBound(data.AllUnitsList) Step 1
            AllUnits(i) = AllDataStructues.Unit.Copy(data.AllUnitsList(i))
            UnitsArrayPos.Add(AllUnits(i).unitID.ToUpper, i)

            unitSubrace = AllUnits(i).race

            If comm.customRace.ContainsKey(AllUnits(i).unitID) Then
                AllUnits(i).race = comm.RaceIdentifierToSubrace(comm.customRace.Item(AllUnits(i).unitID))
            Else
                AllUnits(i).race = comm.RaceIdentifierToSubrace(AllUnits(i).race)
            End If
            ExpBar(i) = AllUnits(i).EXPnext
            ExpKilled(i) = AllUnits(i).EXPkilled
            If AllUnits(i).small Then
                multiplierUnitDesiredStats(i) = comm.defValues.smallUnitsExpMultiplier
            Else
                multiplierUnitDesiredStats(i) = comm.defValues.giantUnitsExpMultiplier
            End If

            If PlayableSubraces.Contains(unitSubrace) Then
                AllUnits(i).fromRaceBranch = True
            Else
                AllUnits(i).fromRaceBranch = False
            End If
        Next i

        ReDim AllItems(UBound(data.AllItemsList)), ItemCostSum(UBound(data.AllItemsList)), Global_ItemsWeightMultiplier(UBound(data.AllItemsList))

        Dim weight As New Dictionary(Of String, String)
        For Each s As String In comm.defValues.Global_ItemType_ChanceMultiplier.Split(CChar(";"))
            Dim i As Integer = s.IndexOf("=")
            weight.Add(s.Substring(0, i).ToUpper, s.Substring(i + 1).ToUpper)
        Next s
        minItemGoldCost = Integer.MaxValue
        For i As Integer = 0 To UBound(AllItems) Step 1
            AllItems(i) = AllDataStructues.Item.Copy(data.AllItemsList(i))
            If AllItems(i).type = GenDefaultValues.ItemTypes.talisman Then
                AllItems(i).itemCost *= data.settings.talismanChargesDefaultAmount
            End If
            ItemsArrayPos.Add(AllItems(i).itemID.ToUpper, i)

            ItemCostSum(i) = AllDataStructues.Cost.Sum(LootCost(AllItems(i)))
            AllItems(i).itemCostSum = ItemCostSum(i)
            Global_ItemsWeightMultiplier(i) = ItemTypeWeight(weight, comm.itemType.Item(AllItems(i).type), ItemCostSum(i))
            If comm.LootItemChanceMultiplier.ContainsKey(AllItems(i).itemID.ToUpper) Then
                Global_ItemsWeightMultiplier(i) *= comm.LootItemChanceMultiplier.Item(AllItems(i).itemID.ToUpper)
            End If
            If AllItems(i).itemCost.Gold > 0 Then minItemGoldCost = Math.Min(minItemGoldCost, CInt(AllItems(i).itemCost.Gold))
        Next i
        bak_multiplierItemsWeight = CType(Global_ItemsWeightMultiplier.Clone, Double())

        ReDim AllSpells(UBound(data.AllSpellsList))
        For i As Integer = 0 To UBound(data.AllSpellsList) Step 1
            AllSpells(i) = AllDataStructues.Spell.Copy(data.AllSpellsList(i))
            SpellsArrayPos.Add(AllSpells(i).spellID.ToUpper, i)
        Next i

        If Not IsNothing(data.AllModificatorsList) Then
            For i As Integer = 0 To UBound(data.AllModificatorsList) Step 1
                AllModificators.Add(data.AllModificatorsList(i).id.ToUpper, data.AllModificatorsList(i))
            Next i
        End If

        Call ResetExclusions()
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
    ''' <summary>Установит множители шанса появления предметов на значения по умолчанию</summary>
    Public Sub ResetItemWeightMultiplier()
        Global_ItemsWeightMultiplier = CType(bak_multiplierItemsWeight.Clone, Double())
    End Sub
    Public Sub ResetAddedItems()
        Global_AddedItems.Clear()
    End Sub
    Private Sub ResetExclusions()
        If Not IsNothing(AllUnits) Then
            For i As Integer = 0 To UBound(AllUnits) Step 1
                If comm.IsExcluded(AllUnits(i)) Then
                    AllUnits(i).useState = GenDefaultValues.ExclusionState.excluded
                Else
                    AllUnits(i).useState = GenDefaultValues.ExclusionState.canUse
                End If
            Next i
        End If
        If Not IsNothing(AllItems) Then
            For i As Integer = 0 To UBound(AllItems) Step 1
                If comm.IsExcluded(AllItems(i)) Then
                    AllItems(i).useState = GenDefaultValues.ExclusionState.excluded
                Else
                    AllItems(i).useState = GenDefaultValues.ExclusionState.canUse
                End If
            Next i
        End If
        'If Not IsNothing(AllSpells) Then
        '    For i As Integer = 0 To UBound(AllSpells) Step 1
        '        If comm.IsExcluded(AllSpells(i)) Then
        '            AllSpells(i).useState = GenDefaultValues.ExclusionState.excluded
        '        Else
        '            AllSpells(i).useState = GenDefaultValues.ExclusionState.canUse
        '        End If
        '    Next i
        'End If
    End Sub

#Region "Find stats"
    ''' <summary>Найдет статы юнита по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxUUxxxx</param>
    Public Function FindUnitStats(ByVal ID As String) As AllDataStructues.Unit
        Dim f As String = ID.ToUpper
        If UnitsArrayPos.ContainsKey(f) Then
            Return AllDataStructues.Unit.Copy(AllUnits(UnitsArrayPos.Item(f)))
        Else
            Return Nothing
        End If
    End Function
    ''' <summary>Найдет статы предмета по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxIGxxxx</param>
    Public Function FindItemStats(ByVal ID As String) As AllDataStructues.Item
        Dim f As String = ID.ToUpper
        If ItemsArrayPos.ContainsKey(f) Then
            Return AllDataStructues.Item.Copy(AllItems(ItemsArrayPos.Item(f)))
        Else
            Return Nothing
        End If
    End Function
    ''' <summary>Найдет статы заклинания по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxSSxxxx</param>
    Public Function FindSpellStats(ByVal ID As String) As AllDataStructues.Spell
        Dim f As String = ID.ToUpper
        If SpellsArrayPos.ContainsKey(f) Then
            Return AllDataStructues.Spell.Copy(AllSpells(SpellsArrayPos.Item(f)))
        Else
            Return Nothing
        End If
    End Function
    ''' <summary>Найдет модификатор по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxUMxxxx</param>
    Public Function FindModificatorStats(ByVal ID As String) As AllDataStructues.Modificator
        Dim f As String = ID.ToUpper
        If AllModificators.ContainsKey(f) Then
            Return AllModificators.Item(f)
        Else
            Return Nothing
        End If
    End Function
#End Region

#Region "Resources conversion"
    ''' <summary>Может быть преобразует часть золота в ману. Результат будет кратен 25</summary>
    ''' <param name="input">Начальные ресурсы. При конвертации начальная мана не пропадет</param>
    ''' <param name="conversionChance">Шанс сконвертировать часть золота в ману (от 0 до 1)</param>
    ''' <param name="conversionAmount">Какую часть золота сконвертировать (от 0 до 1)</param>
    Public Function GoldToMana(ByRef input As AllDataStructues.Cost, ByVal conversionChance As Double, ByVal conversionAmount As Double) As AllDataStructues.Cost
        Dim output As AllDataStructues.Cost = AllDataStructues.Cost.Copy(input)
        If conversionChance > 0 AndAlso AllDataStructues.Cost.Sum(mapData.minesAmount) - mapData.minesAmount.Gold > 0 AndAlso rndgen.Rand(0, 1, True) <= conversionChance Then
            Dim relationships As New AllDataStructues.Cost
            Do While AllDataStructues.Cost.Sum(relationships) = 0
                If mapData.minesAmount.Black > 0 Then relationships.Black = rndgen.RndInt(0, mapData.minesAmount.Black, True)
                If mapData.minesAmount.Blue > 0 Then relationships.Blue = rndgen.RndInt(0, mapData.minesAmount.Blue, True)
                If mapData.minesAmount.Green > 0 Then relationships.Green = rndgen.RndInt(0, mapData.minesAmount.Green, True)
                If mapData.minesAmount.Red > 0 Then relationships.Red = rndgen.RndInt(0, mapData.minesAmount.Red, True)
                If mapData.minesAmount.White > 0 Then relationships.White = rndgen.RndInt(0, mapData.minesAmount.White, True)
            Loop

            Dim manaPiece As Double = input.Gold * Math.Max(Math.Min(conversionAmount, 1), 0) / AllDataStructues.Cost.Sum(relationships)

            Dim roundBy As Integer = 25
            Dim dGold As Double = 0

            Dim order() As Integer = New Integer() {0, 1, 2, 3, 4}
            For n As Integer = 1 To UBound(order) Step 1
                Dim r As Integer = rndgen.RndPos(order.Length, True) - 1
                Dim t As Integer = order(r)
                order(r) = order(0)
                order(0) = t
            Next n
            For n As Integer = 0 To UBound(order) Step 1
                If order(n) = 0 Then
                    Call GoldToManaRound(relationships.Black * manaPiece, roundBy, output, output.Black, dGold)
                ElseIf order(n) = 1 Then
                    Call GoldToManaRound(relationships.Blue * manaPiece, roundBy, output, output.Blue, dGold)
                ElseIf order(n) = 2 Then
                    Call GoldToManaRound(relationships.Green * manaPiece, roundBy, output, output.Green, dGold)
                ElseIf order(n) = 3 Then
                    Call GoldToManaRound(relationships.Red * manaPiece, roundBy, output, output.Red, dGold)
                Else
                    Call GoldToManaRound(relationships.White * manaPiece, roundBy, output, output.White, dGold)
                End If
            Next n
        End If
        Return output
    End Function
    Private Sub GoldToManaRound(ByRef input As Double, ByRef roundBy As Integer, ByRef output As AllDataStructues.Cost, ByRef field As Integer, ByRef dGold As Double)
        If input <= 0 Then Exit Sub
        input = Math.Min(input, output.Gold)
        Dim modulo As Double = input Mod roundBy
        Dim convet As Integer = CInt(input - modulo)
        output.Gold -= convet
        field += convet
        dGold += modulo
        If dGold > 0 And output.Gold >= roundBy Then
            Dim r As Double = rndgen.Rand(0, CDbl(roundBy), True)
            If r < dGold Then
                output.Gold -= roundBy
                field += roundBy
                dGold -= roundBy
            End If
        End If
    End Sub
#End Region

#Region "Creation of gen settings"
    ''' <summary>Вычисляет параметры отряда по составу. Цена предмета в мане прибавится к стоимости лута в золоте</summary>
    ''' <param name="stack">ID юнитов и предметов отряда</param>
    ''' <param name="isSettingsForRuins"> Опция для перегенератора карт.
    ''' Для руин в любом случае генератор лута не будет обязан добавлять предмет в качестве награды (True).
    ''' Для остальных объектов (False) генератор будет обязан добавить хоть какой-то предмет, если
    ''' до перегенерации в награде были предметы, не входящие в список PreservedItems.txt</param>
    Public Function StackStats(ByRef stack As AllDataStructues.Stack, ByVal isSettingsForRuins As Boolean) As AllDataStructues.DesiredStats
        Dim result As New AllDataStructues.DesiredStats _
            With {.Race = New List(Of Integer), _
                  .preservedUnits = New List(Of AllDataStructues.Stack.UnitInfo), _
                  .LeaderModificators = New List(Of String), _
                  .UnitsModificators = New List(Of String), _
                  .ModificatorsEffect = 1}
        Dim unit As AllDataStructues.Unit
        Dim expKilledSum As Double
        Dim currentUnitLevel As Integer
        For i As Integer = 0 To UBound(stack.units) Step 1
            If Not stack.units(i).unit.unitID = GenDefaultValues.emptyItem Then
                unit = FindUnitStats(stack.units(i).unit.unitID)
                If unit.unitID = "" Then Throw New Exception("Неизвестный id юнита: " & stack.units(i).unit.unitID)
                If Not result.Race.Contains(unit.race) Then result.Race.Add(unit.race)
                If unit.unitBranch = GenDefaultValues.UnitClass.leader Then result.WaterOnly = unit.waterOnly
                If Not comm.IsPreserved(unit) Then
                    result.ExpStackKilled += unit.EXPkilled
                    result.ExpBarAverage += unit.EXPnext
                    If unit.small Then
                        result.StackSize += 1
                    Else
                        result.StackSize += 2
                        result.MaxGiants += 1
                    End If
                    If Not unit.small Or unit.reach = GenDefaultValues.UnitAttackReach.melee Then result.MeleeCount += 1
                    currentUnitLevel = Math.Max(stack.units(i).level, unit.level)
                    If Not settings.preserveUnitsOverlevel Then
                        result.ExpStackKilled += unit.GetExpKilledOverlevel(currentUnitLevel)
                        result.ExpBarAverage += unit.GetExpNextOverlevel(currentUnitLevel)
                    Else
                        Dim m As Double = (unit.EXPnext + unit.GetExpNextOverlevel(currentUnitLevel)) / unit.EXPnext
                        result.WeightedOverlevel += (currentUnitLevel - unit.level) * unit.EXPkilled * m
                        expKilledSum += unit.EXPkilled
                    End If
                Else
                    Dim c As AllDataStructues.Stack.UnitInfo = AllDataStructues.Stack.UnitInfo.Copy(stack.units(i))
                    c.isPreserved = True
                    result.preservedUnits.Add(c)
                End If
            End If
        Next i
        If result.StackSize > 0 Then result.ExpBarAverage = CInt(result.ExpBarAverage / result.StackSize)

        For i As Integer = 0 To UBound(stack.units) Step 1
            If Not stack.units(i).unit.unitID = GenDefaultValues.emptyItem Then
                unit = FindUnitStats(stack.units(i).unit.unitID)
                If Not comm.IsPreserved(unit) Then
                    If Not IsNothing(stack.units(i).modificators) Then
                        For Each m As String In stack.units(i).modificators
                            If unit.unitBranch = GenDefaultValues.UnitClass.leader Then
                                result.LeaderModificators.Add(m)
                            Else
                                result.UnitsModificators.Add(m)
                            End If
                        Next m
                        Dim weightSum As Double = result.ExpStackKilled
                        If weightSum > 0 Then
                            Dim modEffect As Double = AllDataStructues.Modificator.UnitPowerChange(stack.units(i), Me)
                            currentUnitLevel = Math.Max(stack.units(i).level, unit.level)
                            Dim weight As Double = unit.EXPkilled + unit.GetExpKilledOverlevel(currentUnitLevel)
                            Call AddModificatorEffect(result.ModificatorsEffect, modEffect, weight, weightSum)
                        End If
                    End If
                End If
            End If
        Next i

        Dim LCost As AllDataStructues.Cost = LootCost(stack.items)
        result.LootCost = AllDataStructues.Cost.Sum(LCost)
        result.IGen = GetItemsGenSettings(stack.items, isSettingsForRuins)
        If expKilledSum > 0 Then result.WeightedOverlevel = result.WeightedOverlevel * result.StackSize / expKilledSum

        Return result
    End Function
    ''' <summary>Определит настройки генерации новых предметов</summary>
    ''' <param name="items">Список предметов объекта</param>
    ''' <param name="isSettingsForRuins"> Опция для перегенератора карт.
    ''' Для руин в любом случае генератор лута не будет обязан добавлять предмет в качестве награды (True).
    ''' Для остальных объектов (False) генератор будет обязан добавить хоть какой-то предмет, если
    ''' до перегенерации в награде были предметы, не входящие в список PreservedItems.txt</param>
    Public Function GetItemsGenSettings(ByRef items As List(Of String), ByVal isSettingsForRuins As Boolean) As AllDataStructues.LootGenSettings
        If IsNothing(items) Then Return New AllDataStructues.LootGenSettings(settings.ApplyStrictTypesFilter)
        Dim result As New AllDataStructues.LootGenSettings(settings.ApplyStrictTypesFilter) With { _
            .ConsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True}, _
            .NonconsumableItems = New AllDataStructues.ItemGenSettings With {.exclude = True}, _
            .JewelItems = New AllDataStructues.ItemGenSettings With {.exclude = True}}
        Call result.SetTypesAmountArray()
        Dim item As AllDataStructues.Item
        For Each id As String In items
            item = FindItemStats(id)
            If Not comm.IsPreserved(item) Then
                result.addLootAnyway = Not isSettingsForRuins
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
                Call result.IncreaseTypeAmount(item)
            Else
                If IsNothing(result.PreserveItems) Then result.PreserveItems = New List(Of String)
                result.PreserveItems.Add(item.itemID.ToUpper)
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

    Private Function RecalculateMultiplier(ByRef pos As Point, ByRef inValue As Double) As Double
        If inValue <= 1 Or IsNothing(pos) Then Return inValue
        Dim r As Double = Double.MaxValue
        Dim halfCapitalSize As Integer = 2
        For i As Integer = 0 To UBound(mapData.capitalPos) Step 1
            r = Math.Min(r, pos.SqDist(New Point(mapData.capitalPos(i).X + halfCapitalSize, mapData.capitalPos(i).Y + halfCapitalSize)))
        Next i
        Dim maxR As Double = comm.defValues.weakerUnitsRadius
        If r < maxR * maxR Then
            r = Math.Sqrt(r)
            Return 1 + (inValue - 1) * r / maxR
        Else
            Return inValue
        End If
    End Function
    Private Sub AddModificatorEffect(ByRef addTo As Double, ByRef effect As Double, _
                                     ByRef weight As Double, ByRef weightSum As Double)
        addTo *= 1 + (effect - 1) * weight / weightSum
    End Sub
#End Region

#Region "Loot info"
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items As List(Of AllDataStructues.Item)) As AllDataStructues.Cost
        Dim result As New AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As AllDataStructues.Item In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items() As AllDataStructues.Item) As AllDataStructues.Cost
        Dim result As New AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As AllDataStructues.Item In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items As List(Of String)) As AllDataStructues.Cost
        Dim result As New AllDataStructues.Cost
        If IsNothing(items) Then Return result
        For Each Item As String In items
            result += LootCost(Item)
        Next Item
        Return result
    End Function
    ''' <summary>Определяет суммарную ценность предметов</summary>
    ''' <param name="items">Список предметов</param>
    Public Function LootCost(ByRef items() As String) As AllDataStructues.Cost
        Dim result As New AllDataStructues.Cost
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
#End Region

#Region "Logging"
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
    Private Sub AddToLog(ByRef LogID As Integer, ByRef DesiredStats As AllDataStructues.DesiredStats)
        If LogID > -1 Then
            Call log.MAdd(LogID, DesiredStats)
        Else
            Call log.Add(DesiredStats)
        End If
    End Sub
    Public Sub AddToLog(ByVal LogID As Integer, ByRef contString As Log.printSelectionList, ByVal v() As AllDataStructues.Unit, ByRef i As List(Of Integer))
        If LogID > -1 Then
            Call log.MAdd(LogID, contString, v, i)
        Else
            Call log.Add(contString, v, i)
        End If
    End Sub
#End Region

#Region "Loot creation"
    ''' <summary>Генерирует набор предметов. В принципе может вернуть пустой список</summary>
    ''' <param name="GenSettings">Общие настройки</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ItemsGen(ByVal GenSettings As AllDataStructues.CommonLootCreationSettings, _
                             Optional ByVal LogID As Integer = -1) As List(Of String)

        Call LootGenPrepare(GenSettings, LogID, True)

        Dim serialExecution As Boolean = (LogID < 0)
        Dim maxCost(), selected As Integer
        Dim weight(UBound(AllItems)) As Double
        Dim IDs As New List(Of Integer)
        Dim result As New List(Of String)

        Dim again As Boolean
        Dim itemsFilter As New ItemsFilter(2, Me, GenItemSetDynIGen(GenSettings.IGen, GenSettings.GoldCost), _
                                           GenSettings.TypeCostRestriction, Nothing, GenSettings.GoldCost)
        itemsFilter.presets(0).useSimple = True
        itemsFilter.presets(0).useTypeStrict = True
        itemsFilter.presets(0).useTypeSoft = True
        itemsFilter.presets(0).useTypeCost = True
        itemsFilter.presets(0).useCostSum = True

        itemsFilter.presets(1).useCostBar = True

        Dim DynWeight As New ItemsWeigtMultipliers(Me, GenSettings, itemsFilter)

        Do While itemsFilter.CurrentMaxLootCost >= minItemGoldCost
            maxCost = GenItemMaxCost(itemsFilter.IGen, itemsFilter.CurrentMaxLootCost)
            itemsFilter.TypeCostBar = GenItemCostBar(itemsFilter.IGen, maxCost, serialExecution)
            Call AddToLog(LogID, "Max cost bar:" & itemsFilter.CurrentMaxLootCost & _
                                 " Selected cost bar:" & itemsFilter.TypeCostBar(0) & "|" & itemsFilter.TypeCostBar(1) & "|" & itemsFilter.TypeCostBar(2) & _
                                 " max item cost:" & maxCost(0) & "|" & maxCost(1) & "|" & maxCost(2))
            IDs.Clear()
            itemsFilter.ForceDisableStrictTypesFilter = False
            again = True
            Do While again
                For i As Integer = 0 To UBound(AllItems) Step 1
                    If itemsFilter.Filter(0, AllItems(i)) Then
                        IDs.Add(i)
                        weight(i) = GenItemWeight(AllItems(i), itemsFilter.TypeCostBar) * _
                                    Global_ItemsWeightMultiplier(i) * _
                                    DynWeight.Nearby_SameTypeMultiplier(AllItems(i).type) * _
                                    DynWeight.Nearby_SameItemMultiplier(i) * _
                                    DynWeight.NearbyCapitalSameTypeItemsEffectOnWeight(i)
                        If Not itemsFilter.Filter(1, AllItems(i)) Then weight(i) *= 0.001
                        weight(i) *= DynWeight.ThisBagItemsEffectOnWeight(result, i)
                    Else
                        weight(i) = 0
                    End If
                Next i
                If IDs.Count > 0 Then
                    again = False
                Else
                    If Not itemsFilter.ForceDisableStrictTypesFilter Then
                        itemsFilter.ForceDisableStrictTypesFilter = True
                    Else
                        again = False
                    End If
                End If
            Loop
            If IDs.Count = 0 Then Exit Do

            selected = comm.RandomSelection(IDs, weight, serialExecution)
            Call DynWeight.ItemAdded(result, selected, True, LogID)
        Loop

        If Not IsNothing(GenSettings.IGen.PreserveItems) AndAlso GenSettings.IGen.PreserveItems.Count > 0 Then
            For Each item As String In GenSettings.IGen.PreserveItems
                selected = ItemsArrayPos.Item(item.ToUpper)
                Call DynWeight.ItemAdded(result, selected, False, LogID)
                Dim thing As AllDataStructues.Item = AllItems(selected)
                Call AddToLog(LogID, "Preserved item:" & thing.name & " id:" & thing.itemID.ToUpper)
            Next item
        End If

        Call AddToLog(LogID, "----Loot creation ended----")

        Return result
    End Function
    ''' <summary>Генерирует набор предметов. В принципе может вернуть пустой список</summary>
    ''' <param name="GenSettings">Общие настройки</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ItemsGen(ByVal GenSettings As AllDataStructues.CommonLootCreationSettings, _
                             ByVal LootCostMultiplier As Double, _
                             Optional ByVal LogID As Integer = -1) As List(Of String)
        Dim s As AllDataStructues.CommonLootCreationSettings = AllDataStructues.CommonLootCreationSettings.Copy(GenSettings)
        Dim lcm As Double = RecalculateMultiplier(GenSettings.pos, LootCostMultiplier)
        s.IGen.lootCostMultiplier = lcm
        s.GoldCost = CInt(s.GoldCost * lcm)
        Return ItemsGen(s, LogID)
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
    Private Sub LootGenPrepare(ByRef GenSettings As AllDataStructues.CommonLootCreationSettings, _
                               ByRef LogID As Integer, _
                               ByRef lootGenCall As Boolean)

        Dim preservedItemsCost As Integer = AllDataStructues.Cost.Sum(LootCost(GenSettings.IGen.PreserveItems))
        If GenSettings.IGen.lootCostMultiplier > 0 Then preservedItemsCost = CInt(preservedItemsCost * GenSettings.IGen.lootCostMultiplier)
        GenSettings.GoldCost -= preservedItemsCost

        Call GenSettings.IGen.Initialize()

        If (IsNothing(GenSettings.IGen.PreserveItems) OrElse GenSettings.IGen.PreserveItems.Count = 0) _
         And GenSettings.IGen.addLootAnyway And GenSettings.GoldCost < minItemGoldCost Then
            GenSettings.GoldCost = CInt(1.2 * minItemGoldCost)
        End If

        If lootGenCall Then
            Call AddToLog(LogID, "----Loot creation started----" & vbNewLine & _
                                 "Gold sum: " & GenSettings.GoldCost)
        Else
            Call AddToLog(LogID, "----Single item creation started----" & vbNewLine & _
                                 "Max cost: " & GenSettings.GoldCost)
        End If
        Call AddToLog(LogID, "Preserved items cost sum: " & preservedItemsCost)

        If Not IsNothing(GenSettings.pos) Then
            Call AddToLog(LogID, "Position: " & GenSettings.pos.X & " " & GenSettings.pos.Y)
        Else
            Call AddToLog(LogID, "Position: unknown")
        End If
        Call AddToLog(LogID, GenSettings.IGen)
    End Sub

    ''' <summary>Генерирует один предмет. Если не получится выбрать подходящий предмет, вернет пустую строку</summary>
    ''' <param name="GenSettings">Общие настройки</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ThingGen(ByVal GenSettings As AllDataStructues.CommonLootCreationSettings, _
                             Optional ByVal LogID As Integer = -1) As String

        Call LootGenPrepare(GenSettings, LogID, False)

        Dim serialExecution As Boolean = (LogID < 0)
        Dim result As String = ""

        Dim itemsFilter As New ItemsFilter(1, Me, GenSettings.IGen, GenSettings.TypeCostRestriction, Nothing, GenSettings.GoldCost)
        itemsFilter.presets(0).useSimple = True
        itemsFilter.presets(0).useTypeStrict = True
        itemsFilter.presets(0).useTypeSoft = True
        itemsFilter.presets(0).useTypeCost = True

        itemsFilter.ForceDisableStrictTypesFilter = False
        itemsFilter.strictMinCost = CInt(Math.Max(Math.Min(comm.defValues.MinRuinsLootCostMultiplier, 1), 0) * GenSettings.GoldCost)
        itemsFilter.strictMaxCost = GenSettings.GoldCost

        Dim DynWeight As New ItemsWeigtMultipliers(Me, GenSettings, itemsFilter)

        If Not IsNothing(GenSettings.IGen.PreserveItems) AndAlso GenSettings.IGen.PreserveItems.Count > 0 Then
            Dim selected As Integer = ItemsArrayPos.Item(GenSettings.IGen.PreserveItems.Item(0).ToUpper)
            Call DynWeight.ItemAdded(result, selected, False, LogID)
            Dim thing As AllDataStructues.Item = AllItems(selected)
            Call AddToLog(LogID, "Preserved item:" & thing.name & " id:" & thing.itemID.ToUpper)
        Else
            Dim IDs As New List(Of Integer)
            Dim weight(UBound(AllItems)) As Double
            Dim again As Boolean = True
            Do While again
                For i As Integer = 0 To UBound(AllItems) Step 1
                    weight(i) = Global_ItemsWeightMultiplier(i) * _
                                DynWeight.Nearby_SameTypeMultiplier(AllItems(i).type) * _
                                DynWeight.Nearby_SameItemMultiplier(i) * _
                                DynWeight.NearbyCapitalSameTypeItemsEffectOnWeight(i)
                    If itemsFilter.Filter(0, AllItems(i)) Then IDs.Add(i)
                Next i
                If IDs.Count > 0 Then
                    again = False
                Else
                    If Not itemsFilter.ForceDisableStrictTypesFilter Then
                        itemsFilter.ForceDisableStrictTypesFilter = True
                    Else
                        again = False
                    End If
                End If
            Loop
            If IDs.Count > 0 Then
                Dim selected As Integer = comm.RandomSelection(IDs, New Double()() {ItemCostSum}, _
                    New Double() {itemsFilter.CurrentMaxLootCost}, weight, itemGenSigma, serialExecution)
                Call DynWeight.ItemAdded(result, selected, True, LogID)
            End If
        End If

        Call AddToLog(LogID, "----Single item creation ended----")

        Return result
    End Function
    ''' <summary>Генерирует один предмет. Если не получится выбрать подходящий предмет, вернет пустую строку</summary>
    ''' <param name="GenSettings">Общие настройки</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function ThingGen(ByVal GenSettings As AllDataStructues.CommonLootCreationSettings, _
                             ByVal LootCostMultiplier As Double, _
                             Optional ByVal LogID As Integer = -1) As String
        Dim lcm As Double = RecalculateMultiplier(GenSettings.pos, LootCostMultiplier)
        Dim gs As AllDataStructues.CommonLootCreationSettings = AllDataStructues.CommonLootCreationSettings.Copy(GenSettings)
        gs.IGen.lootCostMultiplier = lcm
        gs.GoldCost = CInt(gs.GoldCost * lcm)
        Return ThingGen(gs, LogID)
    End Function

    Private Function GenItemSetDynIGen(ByRef IGen As AllDataStructues.LootGenSettings, ByRef GoldCost As Integer) As AllDataStructues.LootGenSettings
        Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(IGen)
        Dim weightsSum As Double
        For i As Integer = 0 To UBound(settings) Step 1
            If Not settings(i).exclude Then weightsSum += settings(i).costPart
        Next i
        If weightsSum > 1.0001 Then Throw New Exception("Invalid cost parts sum: " & weightsSum)
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
        Dim result(UBound(settings)), min, max, n As Integer
        Dim upCost As Integer = CInt(0.75 * minItemGoldCost)
        Dim minN As Integer = 4
        For i As Integer = 0 To UBound(settings) Step 1
            If Not settings(i).exclude Then
                If settings(i).amount > minN Then
                    Dim n1, n2 As Integer
                    If settings(i).amount > 2 * minN Then
                        n1 = settings(i).amount - minN
                        n2 = settings(i).amount
                    Else
                        n1 = minN
                        n2 = settings(i).amount
                    End If
                    n = rndgen.RndInt(n1, n2, True)
                Else
                    n = settings(i).amount
                End If
                Dim div As Double
                If n > 0 Then
                    div = Math.Max(CDbl(n) - 0.1 * CDbl(n - 1) ^ 1.5, 0.8 * CDbl(n))
                Else
                    div = 1
                End If
                If settings(i).amount > 0 And settings(i).dynCostPart > 0 Then
                    max = CInt(Math.Max(minItemGoldCost, (settings(i).dynCostPart + upCost) / div))
                    min = minItemGoldCost + CInt(0.8 * (max - minItemGoldCost))
                ElseIf settings(i).amount > 0 Then
                    max = CInt(Math.Max(minItemGoldCost, (MaxCost(i) + upCost) / div))
                    min = minItemGoldCost + CInt(0.8 * (max - minItemGoldCost))
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
                result = Common.Gauss(AllDataStructues.Cost.Sum(LootCost(item)), _
                                      CostBar(i), multiItemGenSigmaMultiplier * itemGenSigma)
                Exit For
            End If
        Next i
        Return result
    End Function

    Public Class ItemsWeigtMultipliers

        Public randStack As RandStack
        Private comm As Common

        Public GenSettings As AllDataStructues.CommonLootCreationSettings

        Public ItemsFilter As ItemsFilter

        Private ThisBag_SameAddedCount() As Integer
        Private ThisBag_SameTypeAddedCount() As Integer

        Private Nearby_SameItemCount() As Integer
        Public Nearby_SameItemMultiplier() As Double
        Public Nearby_SameTypeMultiplier() As Double

        Private NearbyCapital_SameTypeCount() As Integer = Nothing
        Private NearbyCapital_SameTypeMultiplierSettings() As ItemAmountWeightMultiplierSetting = Nothing
        Private Structure ItemAmountWeightMultiplierSetting
            Public maxAmount As Integer
            Public multiplier As Double
        End Structure

        Public Sub New(ByRef _randStack As RandStack, ByRef _GenSettings As AllDataStructues.CommonLootCreationSettings, _
                       ByRef _ItemsFilter As ItemsFilter)
            randStack = _randStack
            comm = _randStack.comm
            GenSettings = _GenSettings
            ItemsFilter = _ItemsFilter

            ReDim ThisBag_SameAddedCount(UBound(randStack.AllItems)), ThisBag_SameTypeAddedCount(UBound(comm.defValues.ThisBag_SameItemsType_ChanceMultiplier))
            Call SameItemsCounter(GenSettings.pos)
            Call SameItemDynWeight(GenSettings.pos, Nearby_SameItemCount)
            Call ItemTypeDynWeight(GenSettings.pos)
            Call SameTypeAroundCapitalMultiplierSettings()
            Call SameTypesAroundCapitalCounter()
        End Sub
        Private Sub ItemTypeDynWeight(ByRef pos As Point)
            ReDim Nearby_SameTypeMultiplier(comm.defValues.maxItemTypeID)
            For i As Integer = 0 To UBound(Nearby_SameTypeMultiplier) Step 1
                Nearby_SameTypeMultiplier(i) = 1
            Next i
            If IsNothing(pos) Then Exit Sub
            Dim w, t As Double
            Dim d As Double = comm.defValues.AddedItemTypeSearchRadius
            Dim R2 As Double = d * d
            Dim invD As Double = 1 / d
            For Each x As Integer In randStack.Global_AddedItems.Keys
                If Math.Abs(pos.X - x) <= d Then
                    For Each y As Integer In randStack.Global_AddedItems.Item(x).Keys
                        If pos.SqDist(x, y) <= R2 Then
                            w = pos.Dist(x, y) * invD
                            For Each item As AllDataStructues.Item In randStack.Global_AddedItems.Item(x).Item(y)
                                t = comm.defValues.Local_AddedItemType_ChanceMultiplier(item.type)
                                Nearby_SameTypeMultiplier(item.type) *= t + Math.Max((1 - t) * w, 0)
                            Next item
                        End If
                    Next y
                End If
            Next x
        End Sub
        Private Sub SameItemsCounter(ByRef pos As Point)
            ReDim Nearby_SameItemCount(UBound(randStack.AllItems))
            If IsNothing(pos) Then Exit Sub
            Dim d As Double = comm.defValues.AddedItemTypeSearchRadius
            Dim R2 As Double = d * d
            For Each x As Integer In randStack.Global_AddedItems.Keys
                If Math.Abs(pos.X - x) <= d Then
                    For Each y As Integer In randStack.Global_AddedItems.Item(x).Keys
                        If pos.SqDist(x, y) <= R2 Then
                            For Each item As AllDataStructues.Item In randStack.Global_AddedItems.Item(x).Item(y)
                                Nearby_SameItemCount(randStack.ItemsArrayPos.Item(item.itemID)) += 1
                            Next item
                        End If
                    Next y
                End If
            Next x
        End Sub
        Private Sub SameItemDynWeight(ByRef pos As Point, ByRef SameItemsCount() As Integer)
            ReDim Nearby_SameItemMultiplier(UBound(randStack.AllItems))
            For i As Integer = 0 To UBound(Nearby_SameItemMultiplier) Step 1
                Nearby_SameItemMultiplier(i) = 1
            Next i
            If IsNothing(pos) Then Exit Sub
            Dim itemI As Integer
            Dim w, t, a, r As Double
            Dim d As Double = comm.defValues.AddedItemTypeSearchRadius
            Dim R2 As Double = d * d
            Dim invD As Double = 1 / d
            For Each x As Integer In randStack.Global_AddedItems.Keys
                If Math.Abs(pos.X - x) <= d Then
                    For Each y As Integer In randStack.Global_AddedItems.Item(x).Keys
                        If pos.SqDist(x, y) <= R2 Then
                            w = pos.Dist(x, y) * invD
                            For Each item As AllDataStructues.Item In randStack.Global_AddedItems.Item(x).Item(y)
                                itemI = randStack.ItemsArrayPos.Item(item.itemID)
                                a = SameItemsCount(itemI)
                                r = comm.defValues.SameItemsAmountRestriction(item.type)
                                If a >= r Then
                                    t = comm.defValues.Local_SameItem_ChanceMultiplier(item.type)
                                    Nearby_SameItemMultiplier(itemI) *= (t + Math.Max((1 - t) * w, 0)) ^ ((a - r + 1) / (a + 1))
                                End If
                            Next item
                        End If
                    Next y
                End If
            Next x
        End Sub
        Private Sub SameTypesAroundCapitalCounter()
            Dim minSqDist As Double = comm.defValues.CapitalLocationRadius ^ 2
            Dim halfCapitalSize As Integer = 2
            Dim m As Double = Double.MaxValue
            Dim nearestCapital As Point = Nothing
            For Each p As Point In randStack.mapData.capitalPos
                Dim c As Point = New Point(p.X + halfCapitalSize, p.Y + halfCapitalSize)
                Dim d As Double = c.SqDist(GenSettings.pos)
                If d <= minSqDist AndAlso d < m Then
                    m = d
                    nearestCapital = New Point(c.X, c.Y)
                End If
            Next p
            If Not IsNothing(nearestCapital) Then
                ReDim NearbyCapital_SameTypeCount(comm.defValues.maxItemTypeID)
                For Each x As Integer In randStack.Global_AddedItems.Keys
                    If Math.Abs(nearestCapital.X - x) <= comm.defValues.CapitalLocationRadius Then
                        For Each y As Integer In randStack.Global_AddedItems.Item(x).Keys
                            If nearestCapital.SqDist(x, y) <= minSqDist Then
                                For Each item As AllDataStructues.Item In randStack.Global_AddedItems.Item(x).Item(y)
                                    NearbyCapital_SameTypeCount(item.type) += 1
                                Next item
                            End If
                        Next y
                    End If
                Next x
            End If
        End Sub
        Private Sub SameTypeAroundCapitalMultiplierSettings()
            ReDim NearbyCapital_SameTypeMultiplierSettings(comm.defValues.maxItemTypeID)
            Dim txt() As String = comm.defValues.CapitalLocationBasicItems
            Dim splited(), s() As String
            Dim n As Integer
            For i As Integer = 0 To UBound(txt) Step 1
                splited = txt(i).Replace(" ", "").Replace(vbTab, "").Split(CChar("="))
                n = comm.itemTypeID.Item(splited(0))
                s = splited(1).Split(CChar("#"))
                NearbyCapital_SameTypeMultiplierSettings(n) = New ItemAmountWeightMultiplierSetting With { _
                    .maxAmount = CInt(s(0)), .multiplier = CDbl(s(1))}
            Next i
        End Sub

        Public Sub ItemAdded(ByRef result As String, ByRef selected As Integer, ByRef printLogMsg As Boolean, ByRef LogID As Integer)
            result = randStack.AllItems(selected).itemID
            Call ItemAddedEffect(selected, printLogMsg, LogID)
        End Sub
        Public Sub ItemAdded(ByRef result As List(Of String), ByRef selected As Integer, ByRef printLogMsg As Boolean, ByRef LogID As Integer)
            result.Add(randStack.AllItems(selected).itemID)
            Call ItemAddedEffect(selected, printLogMsg, LogID)
        End Sub
        Private Sub ItemAddedEffect(ByRef selected As Integer, ByRef printLogMsg As Boolean, ByRef LogID As Integer)
            'добавленный предмет
            Dim item As AllDataStructues.Item = randStack.AllItems(selected)

            If printLogMsg Then
                Call randStack.AddToLog(LogID, "Selected item:" & item.name & _
                                               " id:" & item.itemID & _
                                               " cost:" & randStack.ItemCostSum(selected))
            End If
            Call GenSettings.IGen.Added(item)

            'меняем параметры генерации
            Call GenItemIGenChange(selected)

            'сохраняем глобальную информацию о добавленных- предметах
            Call AddToAddedItemList(item)

            'глобальное уменьшение шанса создать предмет того же типа
            randStack.Global_ItemsWeightMultiplier(selected) *= comm.defValues.Global_AddedItem_ChanceMultiplier

            'количество одинаковых предметов в текущем наборе
            ThisBag_SameAddedCount(selected) += 1

            'количество одинаковых типов предметов в текущем наборе
            ThisBag_SameTypeAddedCount(item.type) += 1

            'уменьшение шанса создать предмет того же типа, что и расположенные поблизости
            Nearby_SameTypeMultiplier(item.type) *= comm.defValues.Local_AddedItemType_ChanceMultiplier(item.type)

            'уменьшение шанса создать предмет, если поблизости есть такой же предмет
            Nearby_SameItemCount(selected) += 1
            If Nearby_SameItemCount(selected) >= comm.defValues.SameItemsAmountRestriction(item.type) Then
                Nearby_SameItemMultiplier(selected) *= comm.defValues.Local_SameItem_ChanceMultiplier(item.type)
            End If

            'количество одинаковых типов предметов рядом с близлежащей столицей
            If Not IsNothing(NearbyCapital_SameTypeCount) Then
                NearbyCapital_SameTypeCount(item.type) += 1
            End If
        End Sub
        Private Sub GenItemIGenChange(ByRef selected As Integer)
            Dim settings() As AllDataStructues.ItemGenSettings = AllDataStructues.LootGenSettings.ToArray(ItemsFilter.IGen)
            For i As Integer = 0 To UBound(settings) Step 1
                If comm.ItemTypesLists(i).Contains(randStack.AllItems(selected).type) Then
                    If settings(i).amount > 0 Then
                        settings(i).amount -= 1
                        If settings(i).amount = 0 Then
                            Dim weightsSum As Double
                            For j As Integer = 0 To UBound(settings) Step 1
                                If Not settings(j).exclude Then weightsSum += settings(j).costPart
                            Next j
                            If weightsSum > 0 Then
                                settings(i).costPart = 1 - weightsSum
                                settings(i).dynCostPart = CInt(settings(i).costPart * ItemsFilter.CurrentMaxLootCost)
                            End If
                        End If
                    End If
                    If settings(i).dynCostPart > 0 Then
                        settings(i).dynCostPart -= AllDataStructues.Cost.Sum(randStack.LootCost(randStack.AllItems(selected)))
                        If settings(i).dynCostPart <= 0 Then
                            settings(i).dynCostPart = 0
                            For j As Integer = 0 To UBound(settings) Step 1
                                If Not i = j And Not settings(j).exclude Then settings(i).exclude = True
                            Next j
                        End If
                    End If
                    If i = 0 Then
                        ItemsFilter.IGen.ConsumableItems = settings(i)
                    ElseIf i = 1 Then
                        ItemsFilter.IGen.NonconsumableItems = settings(i)
                    ElseIf i = 2 Then
                        ItemsFilter.IGen.JewelItems = settings(i)
                    Else
                        Throw New Exception
                    End If
                    Exit For
                End If
            Next i
            ItemsFilter.CurrentMaxLootCost = CInt(ItemsFilter.CurrentMaxLootCost - randStack.ItemCostSum(selected))
        End Sub
        Private Sub AddToAddedItemList(ByRef item As AllDataStructues.Item)
            Dim pos As Point = GenSettings.pos
            If IsNothing(pos) Then Exit Sub
            If Not randStack.Global_AddedItems.ContainsKey(pos.X) Then _
                randStack.Global_AddedItems.Add(pos.X, New Dictionary(Of Integer, List(Of AllDataStructues.Item)))
            If Not randStack.Global_AddedItems.Item(pos.X).ContainsKey(pos.Y) Then _
                randStack.Global_AddedItems.Item(pos.X).Add(pos.Y, New List(Of AllDataStructues.Item))
            randStack.Global_AddedItems.Item(pos.X).Item(pos.Y).Add(item)
        End Sub

        Public Function ThisBagItemsEffectOnWeight(ByRef addedItems As List(Of String), ByRef i As Integer) As Double
            Dim item As AllDataStructues.Item = randStack.AllItems(i)
            Dim d As Integer
            Dim w As Double = 1
            If addedItems.Contains(item.itemID) Then
                d = 1 + ThisBag_SameAddedCount(i) - comm.defValues.SameItemsAmountRestriction(item.type)
                If d > 0 Then w *= comm.defValues.ThisBag_SameItems_ChanceMultiplier(item.type) ^ d
            End If
            If ThisBag_SameTypeAddedCount(item.type) >= comm.defValues.SameItemsTypeAmountRestriction(item.type) Then
                d = 1 + ThisBag_SameTypeAddedCount(item.type) - comm.defValues.SameItemsTypeAmountRestriction(item.type)
                If d > 0 Then w *= comm.defValues.ThisBag_SameItemsType_ChanceMultiplier(item.type) ^ d
            End If
            Return w
        End Function

        Public Function NearbyCapitalSameTypeItemsEffectOnWeight(ByRef i As Integer) As Double
            Dim w As Double = 1
            If Not IsNothing(NearbyCapital_SameTypeCount) Then
                Dim m As ItemAmountWeightMultiplierSetting = NearbyCapital_SameTypeMultiplierSettings(randStack.AllItems(i).type)
                If NearbyCapital_SameTypeCount(randStack.AllItems(i).type) < m.maxAmount Then
                    w = m.multiplier
                End If
            End If
            Return w
        End Function
    End Class

    Public Class ItemsFilter

        Public randStack As RandStack
        Private comm As Common

        Public IGen As AllDataStructues.LootGenSettings
        Public TypeCostRestriction As Dictionary(Of Integer, AllDataStructues.Restriction)

        Public strictMinCost As Integer = -1
        Public strictMaxCost As Integer = -1

        Public presets(-1) As FiltersPreset

        Public CurrentMaxLootCost As Integer
        Public TypeCostBar() As Integer
        Public ForceDisableStrictTypesFilter As Boolean

        Public Class FiltersPreset
            Public useSimple As Boolean
            Public useTypeStrict As Boolean
            Public useTypeSoft As Boolean
            Public useCostBar As Boolean
            Public useCostSum As Boolean
            Public useTypeCost As Boolean
        End Class

        Public Sub New(ByVal _presetsNumber As Integer, _
                       ByRef _randStack As RandStack, _
                       ByRef _DynIGen As AllDataStructues.LootGenSettings, _
                       ByRef _TypeCostRestriction As Dictionary(Of Integer, AllDataStructues.Restriction), _
                       ByRef _TypeCostBar() As Integer, _
                       ByRef _CurrentMaxLootCost As Integer)
            ReDim presets(_presetsNumber - 1)
            For i As Integer = 0 To UBound(presets) Step 1
                presets(i) = New FiltersPreset
            Next i
            randStack = _randStack
            comm = _randStack.comm
            IGen = _DynIGen
            TypeCostRestriction = _TypeCostRestriction
            CurrentMaxLootCost = _CurrentMaxLootCost
            TypeCostBar = _TypeCostBar
        End Sub

        Public Function Filter(ByVal usePreset As Integer, _
                               ByRef item As AllDataStructues.Item) As Boolean

            Dim settings() As AllDataStructues.ItemGenSettings = Nothing
            Dim itemCostSum As Integer = -1
            Dim subtypeID As Integer = -1
            If presets(usePreset).useTypeSoft _
            Or presets(usePreset).useCostBar _
            Or presets(usePreset).useCostSum Then
                settings = AllDataStructues.LootGenSettings.ToArray(IGen)
                For i As Integer = 0 To UBound(comm.ItemTypesLists) Step 1
                    If comm.ItemTypesLists(i).Contains(item.type) Then
                        subtypeID = i
                    End If
                Next i
            End If

            If presets(usePreset).useCostBar _
            Or presets(usePreset).useCostSum _
            Or presets(usePreset).useTypeCost _
            Or strictMinCost > -1 _
            Or strictMaxCost > -1 Then
                itemCostSum = AllDataStructues.Cost.Sum(randStack.LootCost(item))
            End If

            If Not Simple(usePreset, item) Then Return False
            If Not TypeStrict(usePreset, item) Then Return False
            If Not TypeSoft(usePreset, item, settings, subtypeID) Then Return False
            If Not CostBar(usePreset, item, settings, itemCostSum, subtypeID) Then Return False
            If Not CostSum(usePreset, item, settings, itemCostSum, subtypeID) Then Return False
            If Not TypeCost(usePreset, item, itemCostSum) Then Return False
            If Not StrictCost_Min(itemCostSum) Then Return False
            If Not StrictCost_Max(itemCostSum) Then Return False
            Return True
        End Function

        ''' <summary>Фильтр предметов по тому, является ли предмет исключенным</summary>
        Private Function Simple(ByVal usePreset As Integer, ByRef item As AllDataStructues.Item) As Boolean
            If Not presets(usePreset).useSimple Then Return True
            If Not comm.IsAppropriateItem(item) Then Return False
            Return True
        End Function
        ''' <summary>Фильтр предметов по типу: знамя, артефакт, реликвия, свиток, сфера и т.д.</summary>
        Private Function TypeStrict(ByVal usePreset As Integer, ByRef item As AllDataStructues.Item) As Boolean
            If Not presets(usePreset).useTypeStrict Then Return True
            If ForceDisableStrictTypesFilter Then Return True
            If Not IGen.Filter(item) Then Return False
            Return True
        End Function
        ''' <summary>Фильтр предметов по типу: расходуемый, не расходуемый или драгоценность</summary>
        Private Function TypeSoft(ByVal usePreset As Integer, ByRef item As AllDataStructues.Item, _
                                  ByRef settings() As AllDataStructues.ItemGenSettings, _
                                  ByRef subtypeID As Integer) As Boolean
            If Not presets(usePreset).useTypeSoft Then Return True
            If subtypeID > -1 Then
                If settings(subtypeID).exclude Then Return False
            End If
            Return True
        End Function
        ''' <summary>Фильтр предметов их стоимости относительно выбранной планки цены</summary>
        Private Function CostBar(ByVal usePreset As Integer, ByRef item As AllDataStructues.Item, _
                                 ByRef settings() As AllDataStructues.ItemGenSettings, _
                                 ByRef itemCostSum As Integer, _
                                 ByRef subtypeID As Integer) As Boolean
            If Not presets(usePreset).useCostBar Then Return True
            If subtypeID > -1 Then
                If settings(subtypeID).exclude Then Return False
                If itemCostSum = -1 Then Throw New Exception("Unexpected cost sum")
                If itemCostSum > comm.defValues.CostBarExcessLimit * TypeCostBar(subtypeID) Then Return False
            End If
            Return True
        End Function
        ''' <summary>Фильтр предметов их стоимости относительно текущей доступной максимальной цены</summary>
        Private Function CostSum(ByVal usePreset As Integer, ByRef item As AllDataStructues.Item, _
                                 ByRef settings() As AllDataStructues.ItemGenSettings, _
                                 ByRef itemCostSum As Integer, _
                                 ByRef subtypeID As Integer) As Boolean
            If Not presets(usePreset).useCostSum Then Return True
            If subtypeID > -1 Then
                If settings(subtypeID).exclude Then Return False
                If itemCostSum = -1 Then Throw New Exception("Unexpected cost sum")
                If itemCostSum > comm.defValues.LootCostExcessLimit * CurrentMaxLootCost Then Return False
            End If
            Return True
        End Function
        ''' <summary>Фильтр предметов по стоимости (стоимость находится в диапазоне)</summary>
        Private Function TypeCost(ByVal usePreset As Integer, ByRef item As AllDataStructues.Item, _
                                  ByRef itemCostSum As Integer) As Boolean
            If Not presets(usePreset).useTypeCost Then Return True
            If IsNothing(TypeCostRestriction) Then Return True
            If itemCostSum = -1 Then Throw New Exception("Unexpected cost sum")
            If AllDataStructues.Restriction.CheckValue(CDbl(itemCostSum), _
                                                       TypeCostRestriction.Item(item.type)) Then
                Return True
            Else
                Return False
            End If
        End Function
        ''' <summary>Строгий фильтр по минимальной цене предмета</summary>
        Private Function StrictCost_Min(ByRef itemCostSum As Integer) As Boolean
            If strictMinCost < 0 Then Return True
            If itemCostSum = -1 Then Throw New Exception("Unexpected cost sum")
            If itemCostSum < strictMinCost Then Return False
            Return True
        End Function
        ''' <summary>Строгий фильтр по максимальной цене предмета</summary>
        Private Function StrictCost_Max(ByRef itemCostSum As Integer) As Boolean
            If strictMaxCost < 0 Then Return True
            If itemCostSum = -1 Then Throw New Exception("Unexpected cost sum")
            If itemCostSum > strictMaxCost Then Return False
            Return True
        End Function

    End Class

#End Region

#Region "Stack creation"

    ''' <summary>Создаст отряд  в соответствие с желаемыми параметрами. Не нужно пытаться создать отряд водных жителей на земле</summary>
    ''' <param name="GenSettings">Общие настройки</param>
    Public Function Gen(ByRef GenSettings As AllDataStructues.CommonStackCreationSettings) As AllDataStructues.Stack
        'IO.File.WriteAllText("./stats.txt", pos.X & " " & pos.Y & vbNewLine & _
        '                     AllDataStructues.DesiredStats.Print(StackStats, comm.defValues.RaceNumberToRaceChar))
        If Not IsNothing(GenSettings.StackStats.shopContent) Then Return Nothing

        Dim result As New AllDataStructues.Stack
        Dim DynStackStats As AllDataStructues.DesiredStats = AllDataStructues.DesiredStats.Copy(GenSettings.StackStats)
        DynStackStats.Race.Clear()
        For Each i As Integer In GenSettings.StackStats.Race
            Dim s As Integer = comm.RaceIdentifierToSubrace(i)
            If Not DynStackStats.Race.Contains(s) Then DynStackStats.Race.Add(s)
        Next i

        Call log.Add(vbNewLine & "----Stack creation started----")
        Call log.Add("DeltaLeadership: " & GenSettings.deltaLeadership & " GroundTile: " & GenSettings.groundTile & " NoLeader: " & GenSettings.noLeader)
        If Not IsNothing(GenSettings.pos) Then
            Call log.Add("Position: " & GenSettings.pos.X & " " & GenSettings.pos.Y)
        Else
            Call log.Add("Position: unknown")
        End If
        Call log.Add(DynStackStats)
        If GenSettings.StackStats.HasPreservedLeader And GenSettings.noLeader Then
            Call ThrowStackCreationException("I have preserved leader, but noLeader = True", _
                                             GenSettings, GenSettings.StackStats)
        End If

        If GenSettings.StackStats.StackSize > 0 Or (Not IsNothing(GenSettings.StackStats.preservedUnits) _
                                                    AndAlso GenSettings.StackStats.preservedUnits.Count > 0) Then
            result = GenStackMultithread(GenSettings, DynStackStats)
        Else
            result.leaderPos = -1
            ReDim result.units(UBound(busytransfer))
            For i As Integer = 0 To UBound(result.units) Step 1
                result.units(i) = AllDataStructues.Stack.UnitInfo.CreateEmpty
            Next i
        End If
        Call ApplyOverlevel(result, GenSettings)
        Call ApplyModificators(result, GenSettings)
        result.order = GenSettings.order

        result.items = ItemsGen(New AllDataStructues.CommonLootCreationSettings _
                                With {.GoldCost = DynStackStats.LootCost, _
                                      .IGen = DynStackStats.IGen, _
                                      .TypeCostRestriction = Nothing, _
                                      .pos = GenSettings.pos})

        Call log.Add("----Stack creation ended----")

        Return result
    End Function
    ''' <summary>Создаст отряд  в соответствие с желаемыми параметрами. Не нужно пытаться создать отряд водных жителей на земле</summary>
    ''' <param name="GenSettings">Общие настройки</param>
    ''' <param name="StackStrengthMultiplier">Множитель силы отряда: изменяем опыт за убийство и среднюю планку опыта</param>
    ''' <param name="LootCostMultiplier">Множитель стоимости предметов</param>
    Public Function Gen(ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                        ByVal StackStrengthMultiplier As Double, ByVal LootCostMultiplier As Double) As AllDataStructues.Stack
        Dim ssm As Double = RecalculateMultiplier(GenSettings.pos, StackStrengthMultiplier)
        Dim lcm As Double = RecalculateMultiplier(GenSettings.pos, LootCostMultiplier)

        Dim g As AllDataStructues.CommonStackCreationSettings = AllDataStructues.CommonStackCreationSettings.Copy(GenSettings)

        If Not IsNothing(g.StackStats.preservedUnits) AndAlso g.StackStats.preservedUnits.Count > 0 Then
            Dim pExpKilled, pExpBarAverage As Integer
            Dim hasLeader As Boolean
            For Each u As AllDataStructues.Stack.UnitInfo In g.StackStats.preservedUnits
                pExpKilled += u.unit.EXPkilled
                pExpBarAverage += u.unit.EXPnext
                If u.unit.unitBranch = GenDefaultValues.UnitClass.leader Then hasLeader = True
            Next u
            If GenSettings.StackStats.StackSize = 0 And ssm > 1 Then
                g.deltaLeadership += 1
                If ssm > 2 Then g.deltaLeadership += 1
            End If

            pExpKilled = CInt(g.StackStats.ExpStackKilled * ssm + pExpKilled * (ssm - 1))
            pExpBarAverage = CInt(g.StackStats.ExpBarAverage * ssm + pExpBarAverage * (ssm - 1))
            If hasLeader Then
                g.StackStats.ExpStackKilled = Math.Max(pExpKilled, 0)
                g.StackStats.ExpBarAverage = Math.Max(pExpBarAverage, 0)
            Else
                g.StackStats.ExpStackKilled = Math.Max(pExpKilled, 5)
                g.StackStats.ExpBarAverage = Math.Max(pExpBarAverage, 25)
            End If
        Else
            g.StackStats.ExpStackKilled = Math.Max(CInt(g.StackStats.ExpStackKilled * ssm), 5)
            g.StackStats.ExpBarAverage = Math.Max(CInt(g.StackStats.ExpBarAverage * ssm), 25)
        End If
        g.StackStats.LootCost = CInt(g.StackStats.LootCost * lcm)
        g.StackStats.IGen.lootCostMultiplier = lcm

        Return Gen(g)
    End Function

    Private Function SelectPossibleLeader(ByRef leaderID As Integer, ByRef Tolerance As Double, _
                                          ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                          ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                          ByRef MapLordsRaces As List(Of Integer), _
                                          ByRef preservedSlotsCount As Integer) As Boolean
        If Not comm.IsAppropriateLeader(AllUnits(leaderID)) Then Return False

        If AllUnits(leaderID).fromRaceBranch AndAlso MapLordsRaces.Contains(AllUnits(leaderID).race) Then Return False

        If Not settings.ignoreUnitRace Then
            If Not DynStackStats.Race.Contains(AllUnits(leaderID).race) Then Return False
        End If

        If Not AllUnits(leaderID).small And DynStackStats.MaxGiants = 0 Then Return False
        If AllUnits(leaderID).waterOnly And GenSettings.groundTile Then Return False

        If comm.BigStackUnits.ContainsKey(AllUnits(leaderID).unitID) _
        AndAlso DynStackStats.StackSize < comm.BigStackUnits.Item(AllUnits(leaderID).unitID) Then Return False

        If AllUnits(leaderID).small Then
            If preservedSlotsCount + 1 > maxStackSize Then Return False
        Else
            If preservedSlotsCount + 2 > maxStackSize Then Return False
        End If

        If Not GenSettings.order = "" Then
            MsgBox("нужен текст с названием приказа")
            If Not GenSettings.StackStats.WaterOnly = AllUnits(leaderID).waterOnly Then Return False
        End If

        'Dim mult As Double
        'If AllLeaders(leaderID).small Then
        '    mult = 1
        'Else
        '    mult = 2
        'End If
        'If Math.Abs(AllLeaders(leaderID).EXPnext - mult * StackStats.ExpBarAverage) _
        '    > mult * Tolerance * StackStats.ExpBarAverage Then Return False
        If AllUnits(leaderID).EXPkilled > (1 + Tolerance) * DynStackStats.ExpStackKilled Then Return False
        Return True
    End Function
    Private Function SigmaMultiplier(ByRef stat As AllDataStructues.DesiredStats) As Double
        Return comm.defValues.defaultSigma * (CDbl(stat.StackSize) + 1.25 * CDbl(stat.StackSize * stat.StackSize - 1) + 0.2 * CDbl(stat.MaxGiants))
    End Function
    Private Sub ApplyOverlevel(ByRef stack As AllDataStructues.Stack, ByRef GenSettings As AllDataStructues.CommonStackCreationSettings)
        If GenSettings.StackStats.WeightedOverlevel <= 0 Then Exit Sub
        Dim unitOverlevelCost(UBound(stack.units)), minUnitOverlevelCost As Double
        Dim order(UBound(stack.units)) As Integer
        Dim u As Integer = -1
        Dim k As Integer
        Dim overlevel As Double = GenSettings.StackStats.WeightedOverlevel
        minUnitOverlevelCost = Double.MaxValue
        For i As Integer = 0 To UBound(stack.units) Step 1
            If Not stack.units(i).unit.unitID.ToUpper = GenDefaultValues.emptyItem _
               And Not stack.units(i).isPreserved Then
                u += 1
                order(u) = i
                unitOverlevelCost(i) = (stack.units(i).unit.EXPnext + stack.units(i).unit.GetExpNextOverlevel(stack.units(i).unit.level + 1)) / stack.units(i).unit.EXPnext
            End If
        Next i
        If u < 0 Then Exit Sub
        ReDim Preserve order(u)
        Dim m As Double = GenSettings.StackStats.StackSize / GenSettings.StackStats.ExpStackKilled
        For i As Integer = 0 To u Step 1
            k = order(i)
            unitOverlevelCost(k) *= stack.units(k).unit.EXPkilled * m
            minUnitOverlevelCost = Math.Min(minUnitOverlevelCost, unitOverlevelCost(k))
        Next i
        Dim chance As Double = 1 / order.Length
        Do While True
            Call rndgen.Shuffle(order)
            For i As Integer = 0 To u Step 1
                k = order(i)
                If 2 * overlevel >= unitOverlevelCost(k) AndAlso rndgen.RndDblFast(0, 1) <= chance Then
                    If overlevel >= unitOverlevelCost(k) OrElse unitOverlevelCost(k) * rndgen.RndDblFast(0, 1) > overlevel Then
                        overlevel -= unitOverlevelCost(k)
                        stack.units(k).level += 1
                    End If
                End If
            Next i
            If 2 * overlevel < minUnitOverlevelCost Then Exit Do
            If overlevel < minUnitOverlevelCost AndAlso minUnitOverlevelCost * rndgen.RndDblFast(0, 1) > overlevel Then overlevel = 0
        Loop
    End Sub
    Private Sub ApplyModificators(ByRef stack As AllDataStructues.Stack, ByRef GenSettings As AllDataStructues.CommonStackCreationSettings)
        If stack.leaderPos > -1 And GenSettings.StackStats.LeaderModificators.Count > 0 Then
            For Each m As String In GenSettings.StackStats.LeaderModificators
                stack.units(stack.leaderPos).modificators.Add(m)
            Next m
        End If
        If GenSettings.StackStats.UnitsModificators.Count > 0 Then
            Dim preservedUnit, notPreservedUnit As New List(Of Integer)
            For i As Integer = 0 To UBound(stack.units) Step 1
                If Not stack.units(i).unit.unitID.ToUpper = GenDefaultValues.emptyItem Then
                    If Not stack.units(i).unit.unitBranch = GenDefaultValues.UnitClass.leader Then
                        If stack.units(i).isPreserved Then
                            preservedUnit.Add(i)
                        Else
                            notPreservedUnit.Add(i)
                        End If
                    End If
                End If
            Next i
            If preservedUnit.Count + notPreservedUnit.Count > 0 Then
                Dim rData As New RecursiveApplyModificatorsData(stack, GenSettings, Me, preservedUnit, notPreservedUnit)
                'Dim t0 As Integer = Environment.TickCount
                Call RecursiveApplyModificators(rData, 0)
                'Dim t1 As Integer = Environment.TickCount - t0
                'Dim attempts As Integer = rData.totalAttempts
                For i As Integer = 0 To UBound(rData.modificators) Step 1
                    stack.units(rData.bestResult(i)).modificators.Add(rData.modificators(i).id.ToUpper)
                Next i
            Else
                For Each m As String In GenSettings.StackStats.UnitsModificators
                    stack.units(stack.leaderPos).modificators.Add(m)
                Next m
            End If
        End If
    End Sub
    Private Class RecursiveApplyModificatorsData
        Public units() As AllDataStructues.Stack.UnitInfo
        Public preservedUnit() As Integer
        Public notPreservedUnit() As Integer
        Public modificators() As AllDataStructues.Modificator
        Public currentResult() As Integer
        Public bestResult() As Integer
        Public bestValue As Double
        Public wantFValue As Double
        Public totalAttempts As Integer
        Public forceExit As Boolean
        Public baseUnitStats() As AllDataStructues.UnitBattleNumericValues
        Public baseUnitPower() As Double
        Public baseModificators() As List(Of AllDataStructues.Modificator)
        Public basePowerChange() As Double
        Public unitWeight() As Double
        Public weightSum As Double
        Public PowerChangeSingleMod(,) As Double
        Public multithreadOnN As List(Of Integer)

        Public Sub New()
        End Sub
        Public Sub New(ByRef stack As AllDataStructues.Stack, _
                       ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                       ByRef R As RandStack, ByRef preservedUnitList As List(Of Integer), _
                       ByRef notPreservedUnitList As List(Of Integer))
            units = stack.units
            ReDim modificators(GenSettings.StackStats.UnitsModificators.Count - 1)
            Dim q As Integer = -1
            For Each m As String In GenSettings.StackStats.UnitsModificators
                q += 1
                modificators(q) = R.FindModificatorStats(m)
            Next m
            preservedUnit = preservedUnitList.ToArray
            notPreservedUnit = notPreservedUnitList.ToArray
            ReDim currentResult(UBound(modificators)), _
                  bestResult(UBound(modificators)), _
                  baseUnitStats(UBound(stack.units)), _
                  baseUnitPower(UBound(stack.units)), _
                  baseModificators(UBound(stack.units)), _
                  basePowerChange(UBound(stack.units)), _
                  unitWeight(UBound(stack.units))
            multithreadOnN = New List(Of Integer)
            For i As Integer = 0 To UBound(units) Step 1
                baseModificators(i) = New List(Of AllDataStructues.Modificator)
                If Not stack.units(i).unit.unitID.ToUpper = GenDefaultValues.emptyItem Then
                    baseUnitStats(i) = AllDataStructues.UnitBattleNumericValues.BaseBattleNumericStats(stack.units(i))
                    baseUnitPower(i) = AllDataStructues.UnitBattleNumericValues.UnitPower(baseUnitStats(i))
                    Call AllDataStructues.Modificator.AddModificators(baseModificators(i), stack.units(i).modificators, R)
                    basePowerChange(i) = AllDataStructues.Modificator.UnitPowerChange(stack.units(i), R)
                    unitWeight(i) = units(i).unit.EXPkilled + units(i).unit.GetExpKilledOverlevel(units(i).level)
                    weightSum += unitWeight(i)
                End If
            Next i
            bestValue = -1
            wantFValue = GenSettings.StackStats.ModificatorsEffect

            ReDim PowerChangeSingleMod(UBound(stack.units), UBound(modificators))
            Dim unitPos As Integer
            Dim v As Double
            For n As Integer = 0 To UBound(modificators) Step 1
                For i As Integer = 0 To UBound(notPreservedUnit) Step 1
                    unitPos = notPreservedUnit(i)
                    v = AllDataStructues.Modificator.UnitPowerChange(units(unitPos).unit, _
                                                                     baseUnitStats(unitPos), _
                                                                     baseUnitPower(unitPos), _
                                                                     baseModificators(unitPos), _
                                                                     basePowerChange(unitPos), _
                                                                     modificators(n))
                    PowerChangeSingleMod(unitPos, n) = v
                Next i
                For i As Integer = 0 To UBound(preservedUnit) Step 1
                    unitPos = preservedUnit(i)
                    v = AllDataStructues.Modificator.UnitPowerChange(units(unitPos).unit, _
                                                                     baseUnitStats(unitPos), _
                                                                     baseUnitPower(unitPos), _
                                                                     baseModificators(unitPos), _
                                                                     basePowerChange(unitPos), _
                                                                     modificators(n))
                    PowerChangeSingleMod(unitPos, n) = v
                Next i
            Next n

            Dim threadsN(UBound(modificators)) As Integer
            For n As Integer = 0 To UBound(modificators) Step 1
                For i As Integer = 0 To UBound(notPreservedUnit) Step 1
                    unitPos = notPreservedUnit(i)
                    If Not PowerChangeSingleMod(unitPos, n) = 1 Then
                        threadsN(n) += 1
                    End If
                Next i
                If threadsN(n) = 0 Then
                    For i As Integer = 0 To UBound(preservedUnit) Step 1
                        unitPos = preservedUnit(i)
                        If Not PowerChangeSingleMod(unitPos, n) = 1 Then
                            threadsN(n) += 1
                        End If
                    Next i
                End If
                If threadsN(n) = 0 Then
                    threadsN(n) = 1
                End If
            Next n
            Dim nAttempts As Integer = 1
            Dim threads As Integer = 1
            For n As Integer = UBound(modificators) To 0 Step -1
                nAttempts *= threadsN(n)
                If nAttempts > 1000 And threads < Environment.ProcessorCount Then
                    threads *= threadsN(n)
                    multithreadOnN.Add(n)
                End If
            Next n
            If multithreadOnN.Count = 0 Then
                For n As Integer = 0 To UBound(modificators) Step 1
                    If threadsN(n) > 1 Then
                        multithreadOnN.Add(n)
                        Exit For
                    End If
                Next n
            End If
        End Sub

        Public Function FastCopy() As RecursiveApplyModificatorsData
            Dim r As New RecursiveApplyModificatorsData
            r.units = units
            r.preservedUnit = preservedUnit
            r.notPreservedUnit = notPreservedUnit
            r.modificators = modificators
            r.currentResult = CType(currentResult.Clone, Integer())
            r.bestResult = CType(bestResult.Clone, Integer())
            r.bestValue = bestValue
            r.wantFValue = wantFValue
            r.totalAttempts = totalAttempts
            r.forceExit = forceExit
            r.baseUnitStats = baseUnitStats
            r.baseUnitPower = baseUnitPower
            r.baseModificators = baseModificators
            r.basePowerChange = basePowerChange
            r.unitWeight = unitWeight
            r.weightSum = weightSum
            r.PowerChangeSingleMod = PowerChangeSingleMod
            r.multithreadOnN = multithreadOnN
            Return r
        End Function

    End Class
    Private Sub RecursiveApplyModificators(ByRef rData As RecursiveApplyModificatorsData, ByVal n As Integer)
        If rData.forceExit Then Exit Sub
        If n <= UBound(rData.modificators) Then
            Dim skipPreserved As Boolean = False
            Dim skipRandom As Boolean = False
            Call RecurseLayer(rData, n, rData.notPreservedUnit, skipPreserved, skipRandom)
            If Not skipPreserved Then
                Call RecurseLayer(rData, n, rData.preservedUnit, skipPreserved, skipRandom)
            End If
            If Not skipRandom Then
                Dim r As Integer = rndgen.RndIntFast(0, rData.notPreservedUnit.Length + rData.preservedUnit.Length - 1)
                Dim unitPos As Integer
                If r > UBound(rData.notPreservedUnit) Then
                    unitPos = rData.preservedUnit(r - UBound(rData.notPreservedUnit))
                Else
                    unitPos = rData.notPreservedUnit(r)
                End If
                rData.currentResult(n) = unitPos
                Call RecursiveApplyModificators(rData, n + 1)
            End If
        Else
            rData.totalAttempts += 1
            Dim total As Double = 1
            Dim v As Double
            Dim mods(UBound(rData.units)) As List(Of AllDataStructues.Modificator)
            For i As Integer = 0 To UBound(mods) Step 1
                mods(i) = New List(Of AllDataStructues.Modificator)
            Next i
            For i As Integer = 0 To UBound(rData.currentResult) Step 1
                mods(rData.currentResult(i)).Add(rData.modificators(i))
            Next i
            For i As Integer = 0 To UBound(mods) Step 1
                v = AllDataStructues.Modificator.UnitPowerChange(rData.units(i).unit, _
                                                                 rData.baseUnitStats(i), _
                                                                 rData.baseUnitPower(i), _
                                                                 rData.baseModificators(i), _
                                                                 rData.basePowerChange(i), _
                                                                 mods(i))
                Call AddModificatorEffect(total, v, rData.unitWeight(i), rData.weightSum)
            Next i
            Dim replace As Boolean
            If rData.bestValue < 0 Then
                replace = True
            ElseIf Math.Abs(rData.wantFValue - total) < Math.Abs(rData.wantFValue - rData.bestValue) Then
                replace = True
            ElseIf Math.Abs(rData.wantFValue - total) = Math.Abs(rData.wantFValue - rData.bestValue) Then
                If rndgen.RndDblFast(0, 1) > 0.5 Then replace = True
            Else
                replace = False
            End If
            If replace Then
                rData.bestValue = total
                For i As Integer = 0 To UBound(rData.currentResult) Step 1
                    rData.bestResult(i) = rData.currentResult(i)
                Next i
            End If

            Dim d As Double = Math.Abs(rData.bestValue / rData.wantFValue - 1)
            If (d < 0.001 And rData.totalAttempts > 1000) _
            OrElse (d < 0.005 And rData.totalAttempts > 10000) _
            OrElse (d < 0.01 And rData.totalAttempts > 100000) _
            OrElse (d < 0.1 And rData.totalAttempts > 500000) _
            OrElse (rData.totalAttempts > 1000000) Then
                rData.forceExit = True
            End If
        End If
    End Sub
    Private Sub RecurseLayer(ByRef rData As RecursiveApplyModificatorsData, ByVal n As Integer, _
                             ByVal unitPos() As Integer, ByRef skipPreserved As Boolean, ByRef skipRandom As Boolean)
        If rData.multithreadOnN.Contains(n) Then
            Dim pRData(UBound(unitPos)) As RecursiveApplyModificatorsData
            For i As Integer = 0 To UBound(unitPos) Step 1
                If Not rData.PowerChangeSingleMod(unitPos(i), n) = 1 Then
                    skipPreserved = True
                    skipRandom = True
                    Exit For
                End If
            Next i
            Dim trData As RecursiveApplyModificatorsData = rData
            Parallel.For(0, unitPos.Length, _
             Sub(i As Integer)
                 If Not trData.PowerChangeSingleMod(unitPos(i), n) = 1 Then
                     pRData(i) = trData.FastCopy
                     pRData(i).currentResult(n) = unitPos(i)
                     Call RecursiveApplyModificators(pRData(i), n + 1)
                 End If
             End Sub)
            Dim parallelAttempts As Integer = 0
            For i As Integer = 0 To UBound(unitPos) Step 1
                If Not IsNothing(pRData(i)) Then
                    rData.forceExit = pRData(i).forceExit
                    parallelAttempts += pRData(i).totalAttempts - rData.totalAttempts
                    If Math.Abs(rData.wantFValue - pRData(i).bestValue) <= Math.Abs(rData.wantFValue - rData.bestValue) Then
                        rData.bestValue = pRData(i).bestValue
                        For k As Integer = 0 To UBound(rData.currentResult) Step 1
                            rData.bestResult(k) = pRData(i).bestResult(k)
                        Next k
                    End If
                End If
            Next i
            rData.totalAttempts += parallelAttempts
        Else
            For i As Integer = 0 To UBound(unitPos) Step 1
                If Not rData.PowerChangeSingleMod(unitPos(i), n) = 1 Then
                    skipPreserved = True
                    skipRandom = True
                    rData.currentResult(n) = unitPos(i)
                    Call RecursiveApplyModificators(rData, n + 1)
                End If
            Next i
        End If
    End Sub

    Private Function GenStackMultithread(ByVal GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                         ByVal BakDynStackStats As AllDataStructues.DesiredStats) As AllDataStructues.Stack

        Dim units(11)() As AllDataStructues.Stack.UnitInfo
        Dim DynStackStats(UBound(units)) As AllDataStructues.DesiredStats
        Call log.MRedim(units.Length)
        Dim leaderExpKilled() As Integer = Nothing
        Dim MapLordsRaces As New List(Of Integer)
        If Not IsNothing(mapData.mapLords) And Not settings.addUnitsFromBranchesToStacks Then
            For i As Integer = 0 To UBound(mapData.mapLords) Step 1
                MapLordsRaces.Add(comm.LordsRace(mapData.mapLords(i).ToUpper))
            Next i
        End If

        If Not GenSettings.noLeader Then ReDim leaderExpKilled(UBound(units))

        Parallel.For(0, units.Length, _
        Sub(jobID As Integer)
            'For jobID As Integer = 0 To UBound(units) Step 1
            log.MAdd(jobID, "--------Attempt " & jobID + 1 & " started--------")
            Dim FreeMeleeSlots As Integer = 3
            Dim BaseStackSize As Integer
            Dim preservedSlots As Integer = GenSettings.StackStats.PreservedSlots
            DynStackStats(jobID) = AllDataStructues.DesiredStats.Copy(BakDynStackStats)

            Dim SelectedLeader As AllDataStructues.Stack.UnitInfo = GenLeader( _
                   GenSettings, DynStackStats(jobID), FreeMeleeSlots, _
                   MapLordsRaces, CDbl(jobID / units.Length), jobID)
            BaseStackSize = DynStackStats(jobID).StackSize

            If Not GenSettings.noLeader And Not GenSettings.StackStats.HasPreservedLeader Then
                If SelectedLeader.unit.small Then
                    BaseStackSize += 1
                Else
                    BaseStackSize += 2
                End If
            End If

            If BaseStackSize + preservedSlots > maxStackSize Then
                Dim dL As Integer = BaseStackSize + preservedSlots - maxStackSize
                BaseStackSize = Math.Max(BaseStackSize - dL, 0)
                DynStackStats(jobID).StackSize = Math.Max(DynStackStats(jobID).StackSize - dL, 0)
            End If
            If DynStackStats(jobID).StackSize + preservedSlots > maxStackSize Then
                Dim dL As Integer = DynStackStats(jobID).StackSize + preservedSlots - maxStackSize
                BaseStackSize = Math.Max(BaseStackSize - dL, 0)
                DynStackStats(jobID).StackSize = Math.Max(DynStackStats(jobID).StackSize - dL, 0)
            End If

            Dim deltaExpKilled As Integer = 0
            Dim SelectedFighters As List(Of AllDataStructues.Stack.UnitInfo) _
                = GenFingters(GenSettings, DynStackStats(jobID), FreeMeleeSlots, _
                              SelectedLeader, BaseStackSize, _
                              MapLordsRaces, deltaExpKilled, _
                              CDbl(jobID / units.Length), jobID)
            units(jobID) = GenUnitsList(SelectedFighters, SelectedLeader)

            Dim slotsInUse As Integer = 0
            For Each u As AllDataStructues.Stack.UnitInfo In units(jobID)
                If u.unit.small Then
                    slotsInUse += 1
                Else
                    slotsInUse += 2
                End If
            Next u
            If slotsInUse > maxStackSize Then ThrowStackCreationException("Unexpected slots used: " & slotsInUse, _
                                                                          GenSettings, DynStackStats(jobID))
            If Not GenSettings.StackStats.ExpStackKilled = 0 Then
                DynStackStats(jobID).LootCost = CInt(CDbl(DynStackStats(jobID).LootCost) _
                                                     * (1 + CDbl(deltaExpKilled) / GenSettings.StackStats.ExpStackKilled))
            End If

            If Not GenSettings.noLeader Then leaderExpKilled(jobID) = SelectedLeader.unit.EXPkilled

            log.MAdd(jobID, "--------Attempt " & jobID + 1 & " ended--------")
            'Next jobID
        End Sub)

        Call log.Add(log.MPrintAll())
        Call log.MRedim(0)

        Dim selected As Integer = SelectStack(GenSettings, DynStackStats, leaderExpKilled)

        If log.IsEnabled Then
            Dim txt As String = ""
            For Each u As AllDataStructues.Stack.UnitInfo In units(selected)
                txt &= vbNewLine & u.unit.unitID & " " & u.unit.name
            Next u
            Call log.Add("--------Selected Stack--------" & txt)
        End If

        Return GenPositions(GenSettings, DynStackStats(selected), units(selected))
    End Function
    Private Function GenLeader(ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                               ByRef DynStackStats As AllDataStructues.DesiredStats, _
                               ByRef FreeMeleeSlots As Integer, _
                               ByRef MapLordsRaces As List(Of Integer), _
                               ByRef Bias As Double, ByRef LogID As Integer) As AllDataStructues.Stack.UnitInfo
        If GenSettings.noLeader Then Return Nothing

        'здесь проверяем, есть ли сохраненный лидер
        If GenSettings.StackStats.HasPreservedLeader Then
            Dim removeAt As Integer = -1
            For i As Integer = 0 To DynStackStats.preservedUnits.Count - 1 Step 1
                If DynStackStats.preservedUnits.Item(i).unit.unitBranch = GenDefaultValues.UnitClass.leader Then
                    removeAt = i
                    Exit For
                End If
            Next i
            Dim u As AllDataStructues.Stack.UnitInfo = AllDataStructues.Stack.UnitInfo.Copy(DynStackStats.preservedUnits.Item(removeAt))
            DynStackStats.preservedUnits.RemoveAt(removeAt)
            Return u
        End If

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
        Dim preservedSlots As Integer = GenSettings.StackStats.PreservedSlots
        Dim Tolerance As Double = 0.02 * (DynStackStats.StackSize - 1)
        Do While PossibleLeaders.Count < 3
            PossibleLeaders.Clear()
            For i As Integer = 0 To UBound(AllUnits) Step 1
                If SelectPossibleLeader(i, Tolerance, GenSettings, DynStackStats, MapLordsRaces, preservedSlots) Then PossibleLeaders.Add(i)
            Next i
            If Tolerance > 2 And PossibleLeaders.Count > 0 Then Exit Do

            'If Tolerance * DynStackStats.ExpBarAverage > maxExpBar And Tolerance * DynStackStats.ExpStackKilled > maxExpStrackKilled Then
            If Tolerance * DynStackStats.ExpStackKilled > maxExpStrackKilled Then
                If DynStackStats.MaxGiants < 1 Then
                    DynStackStats.MaxGiants = 1
                    Tolerance = 0.02 * (DynStackStats.StackSize - 1)
                Else
                    If PossibleLeaders.Count > 0 Then Exit Do
                    Call ThrowStackCreationException("Something is wrong with the choice of possible stack leaders", _
                                                     GenSettings, DynStackStats)
                End If
            End If
            Tolerance += 0.2
        Loop

        Call AddToLog(LogID, AddressOf PrintSelectionList, AllUnits, PossibleLeaders)
        Dim bar() As Double = SelectBar({DynStackStats.ExpBarAverage}, PossibleLeaders, {ExpBar}, Bias)
        SelectedLeader = comm.RandomSelection(PossibleLeaders, {ExpBar}, bar, _
                                              multiplierUnitDesiredStats, SigmaMultiplier(DynStackStats), serialExecution)

        If SelectedLeader = -1 Then
            Call ThrowStackCreationException("Possibly an endless loop in a random selection from an array of possible leaders", _
                                             GenSettings, DynStackStats)
        End If

        'теперь нужно добрать воинов в отряд
        Dim R As Double = rndgen.Rand(0, 1, serialExecution)
        Dim leadershipCap As Integer = Math.Min(AllUnits(SelectedLeader).leadership + GenSettings.deltaLeadership, maxStackSize)
        If AllUnits(SelectedLeader).small Then
            leadershipCap = Math.Max(leadershipCap, 1)
        Else
            leadershipCap = Math.Max(leadershipCap, 2)
        End If
        If R < 0.1 Then
            If comm.BigStackUnits.ContainsKey(AllUnits(SelectedLeader).unitID) Then
                If DynStackStats.StackSize > comm.BigStackUnits.Item(AllUnits(SelectedLeader).unitID) Then
                    DynStackStats.StackSize -= 1
                End If
            Else
                DynStackStats.StackSize -= 1
            End If
        ElseIf R > 0.9 Then
            DynStackStats.StackSize += 1
            If DynStackStats.StackSize - DynStackStats.MeleeCount < secondrow.Length Then DynStackStats.MeleeCount += 1
        End If
        If AllUnits(SelectedLeader).small Then
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 1)
        Else
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 2)
        End If
        DynStackStats.StackSize = Math.Min(DynStackStats.StackSize, leadershipCap)
        DynStackStats.MeleeCount = Math.Min(DynStackStats.MeleeCount, 3)
        DynStackStats.MaxGiants = Math.Min(DynStackStats.MaxGiants, 3)
        DynStackStats.ExpBarAverage = CInt((DynStackStats.ExpBarAverage * GenSettings.StackStats.StackSize) / DynStackStats.StackSize)

        Call ChangeLimit(AllUnits(SelectedLeader), DynStackStats, FreeMeleeSlots, LogID)

        Return New AllDataStructues.Stack.UnitInfo(AllUnits(SelectedLeader).unitID, _
                                                   AllUnits(SelectedLeader).level, _
                                                   Nothing, _
                                                   Me)
    End Function
    Private Function GenFingters(ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                 ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                 ByRef FreeMeleeSlots As Integer, _
                                 ByRef SelectedLeader As AllDataStructues.Stack.UnitInfo, _
                                 ByRef BaseStackSize As Integer, _
                                 ByRef MapLordsRaces As List(Of Integer), _
                                 ByRef output_delta_expKilled As Integer, _
                                 ByRef Bias As Double, ByRef LogID As Integer) As List(Of AllDataStructues.Stack.UnitInfo)
        Dim SelectedFighters As New List(Of AllDataStructues.Stack.UnitInfo)
        Dim fighter As AllDataStructues.Stack.UnitInfo
        Dim deltaExpKilledIncrement As Integer = 10
        Dim resetStackSettings As Boolean
        Do While SelectedFighters.Count = 0
            Do While DynStackStats.StackSize > 0
                'создаем список воинов, которых можно использовать
                fighter = SelectFighters(False, False, GenSettings, DynStackStats, FreeMeleeSlots, SelectedLeader, _
                                         SelectedFighters, BaseStackSize, MapLordsRaces, Bias, LogID)
                If IsNothing(fighter) Then
                    fighter = SelectFighters(True, False, GenSettings, DynStackStats, FreeMeleeSlots, SelectedLeader, _
                                             SelectedFighters, BaseStackSize, MapLordsRaces, Bias, LogID)
                    If IsNothing(fighter) Then
                        fighter = SelectFighters(True, True, GenSettings, DynStackStats, FreeMeleeSlots, SelectedLeader, _
                                                 SelectedFighters, BaseStackSize, MapLordsRaces, Bias, LogID)
                    End If
                End If
                If IsNothing(fighter) Then
                    If DynStackStats.MeleeCount > 0 Then
                        DynStackStats.MeleeCount = 0
                    Else
                        Exit Do
                    End If
                Else
                    SelectedFighters.Add(fighter)
                End If
                If resetStackSettings And SelectedFighters.Count > 0 Then Exit Do
            Loop
            If GenSettings.noLeader Then
                If SelectedFighters.Count = 0 Then
                    output_delta_expKilled += deltaExpKilledIncrement
                    DynStackStats.ExpStackKilled += deltaExpKilledIncrement
                    If DynStackStats.ExpStackKilled > 9999 Then
                        DynStackStats.ExpStackKilled -= output_delta_expKilled
                        output_delta_expKilled = 0
                        resetStackSettings = True
                    End If
                    If resetStackSettings Then
                        DynStackStats.MeleeCount = 1
                        DynStackStats.MaxGiants = 1
                        DynStackStats.StackSize = 2
                    End If
                End If
            Else
                Exit Do
            End If
        Loop
        If Not IsNothing(DynStackStats.preservedUnits) AndAlso DynStackStats.preservedUnits.Count > 0 Then
            For Each u As AllDataStructues.Stack.UnitInfo In DynStackStats.preservedUnits
                SelectedFighters.Add(AllDataStructues.Stack.UnitInfo.Copy(u))
            Next u
        End If
        Return SelectedFighters
    End Function
    Private Function GenUnitsList(ByRef SelectedFighters As List(Of AllDataStructues.Stack.UnitInfo), _
                                  ByRef SelectedLeader As AllDataStructues.Stack.UnitInfo) As AllDataStructues.Stack.UnitInfo()
        Dim SelectedUnits() As AllDataStructues.Stack.UnitInfo
        Dim n As Integer
        If IsNothing(SelectedLeader) Then
            ReDim SelectedUnits(SelectedFighters.Count - 1)
        Else
            ReDim SelectedUnits(SelectedFighters.Count)
        End If
        If Not IsNothing(SelectedLeader) Then
            SelectedUnits(0) = AllDataStructues.Stack.UnitInfo.Copy(SelectedLeader)
            n = 0
        Else
            n = -1
        End If
        For Each item As AllDataStructues.Stack.UnitInfo In SelectedFighters
            n += 1
            SelectedUnits(n) = AllDataStructues.Stack.UnitInfo.Copy(item)
        Next item
        Return SelectedUnits
    End Function
    Private Function GenPositions(ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                  ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                  ByRef SelectedUnits() As AllDataStructues.Stack.UnitInfo) As AllDataStructues.Stack
        Dim result As New AllDataStructues.Stack With {.leaderPos = -1}
        ReDim result.units(UBound(busytransfer))
        Dim unitIsUsed(UBound(SelectedUnits)) As Boolean
        Dim firstRowSlots As Integer = 3
        Dim secondRowSlots As Integer = 3

        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And Not SelectedUnits(i).unit.small Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And SelectedUnits(i).unit.reach = GenDefaultValues.UnitAttackReach.melee Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And Not SelectedUnits(i).unit.reach = GenDefaultValues.UnitAttackReach.melee Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) Then
                unitIsUsed(i) = SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, True, result)
            End If
        Next i
        For i As Integer = 0 To UBound(unitIsUsed) Step 1
            If Not unitIsUsed(i) Then Call ThrowStackCreationException("Something wrong in the units placer", _
                                                                        GenSettings, DynStackStats)
        Next i
        For i As Integer = 0 To UBound(result.units) Step 1
            If IsNothing(result.units(i)) Then result.units(i) = AllDataStructues.Stack.UnitInfo.CreateEmpty
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
    Private Function SelectStack(ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                 ByRef DynStackStats() As AllDataStructues.DesiredStats, _
                                 ByRef leaderExpKilled() As Integer) As Integer
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
                If Math.Abs(DynStackStats(i).ExpStackKilled) <= ExpTolerance * GenSettings.StackStats.ExpStackKilled Then possible2.Add(i)
            Next i
            If possible2.Count >= 0.2 * DynStackStats.Length Then Exit Do
            If possible2.Count > 0 And ExpTolerance >= 0.1 Then Exit Do
        Loop
        Dim leaderExpKilledMaxValue As Integer = 0
        Dim maxExpDelta As Integer = 0
        If Not IsNothing(leaderExpKilled) Then
            For Each i As Integer In possible2
                leaderExpKilledMaxValue = Math.Max(leaderExpKilledMaxValue, leaderExpKilled(i))
            Next i
        End If
        For Each i As Integer In possible2
            maxExpDelta = Math.Max(maxExpDelta, Math.Abs(DynStackStats(i).ExpStackKilled))
        Next i

        Dim weight(UBound(DynStackStats)) As Double
        For Each i As Integer In possible2
            Dim m As Double
            If Not IsNothing(leaderExpKilled) Then
                m = leaderExpKilled(i) / leaderExpKilledMaxValue
            Else
                m = 1
            End If
            'weight(i) = m / (1 + Math.Abs(DynStackStats(i).ExpStackKilled)) 'old
            weight(i) = m / (1 + (1 + Math.Abs(DynStackStats(i).ExpStackKilled)) / (1 + maxExpDelta)) 'new
        Next i
        Dim selected As Integer = comm.RandomSelection(possible2, weight, False)
        Return selected
    End Function
    Private Sub ThrowStackCreationException(ByRef comment As String, _
                                            ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                            ByRef DynStackStats As AllDataStructues.DesiredStats)
        Dim p As String
        If IsNothing(GenSettings.pos) Then
            p = "unknown"
        Else
            p = "X = " & GenSettings.pos.X & "  Y = " & GenSettings.pos.Y
        End If
        Throw New Exception(comment & vbNewLine & _
                            "Location name: " & GenSettings.StackStats.LocationName & vbNewLine & _
                            "Is ground: " & GenSettings.groundTile & vbNewLine & _
                            "Is template: " & GenSettings.isTemplate & vbNewLine & _
                            "Orfder: " & GenSettings.order & vbNewLine & _
                            "Position: " & p & vbNewLine & _
                            "StackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(GenSettings.StackStats, comm.defValues.RaceNumberToRaceChar) & vbNewLine & _
                            "DynStackStats:" & vbNewLine & AllDataStructues.DesiredStats.Print(DynStackStats, comm.defValues.RaceNumberToRaceChar))
    End Sub

    Private Function SelectFighters(ByRef skipfilter1 As Boolean, ByRef skipfilter2 As Boolean, _
                                    ByRef GenSettings As AllDataStructues.CommonStackCreationSettings, _
                                    ByRef DynStackStats As AllDataStructues.DesiredStats, ByRef FreeMeleeSlots As Integer, _
                                    ByRef SelectedLeader As AllDataStructues.Stack.UnitInfo, _
                                    ByRef SelectedFighters As List(Of AllDataStructues.Stack.UnitInfo), _
                                    ByRef BaseStackSize As Integer, ByRef MapLordsRaces As List(Of Integer), _
                                    ByRef Bias As Double, ByRef LogID As Integer) As AllDataStructues.Stack.UnitInfo

        Dim serialExecution As Boolean = (LogID < 0)
        Dim PossibleFighters As New List(Of Integer)
        'Dim TExpStack As Double = DynStackStats.ExpStackKilled / DynStackStats.StackSize
        Dim SelectedFighter As AllDataStructues.Stack.UnitInfo = Nothing
        'Dim nloops As Integer = 0
        'Do While PossibleFighters.Count = 0 'And TExpStack < 1.1 * DynStackStats.ExpStackKilled
        For j As Integer = 0 To UBound(AllUnits) Step 1
            If SelectPossibleFighter(skipfilter1, skipfilter2, j, DynStackStats, FreeMeleeSlots, _
                                     SelectedLeader, SelectedFighters, BaseStackSize, MapLordsRaces) Then PossibleFighters.Add(j)
        Next j
        '    TExpStack += 0.1 * DynStackStats.ExpStackKilled / DynStackStats.StackSize
        '    nloops += 1
        '    If nloops > 10 Then Exit Do
        'Loop
        If PossibleFighters.Count > 0 Then
            Call AddToLog(LogID, AddressOf PrintSelectionList, AllUnits, PossibleFighters)

            Dim bar() As Double = SelectBar({DynStackStats.ExpBarAverage, CDbl(DynStackStats.ExpStackKilled) / CDbl(DynStackStats.StackSize)}, _
                                            PossibleFighters, {ExpBar, ExpKilled}, Bias)
            Dim selectedID As Integer = comm.RandomSelection(PossibleFighters, {ExpBar, ExpKilled}, _
                                                             bar, multiplierUnitDesiredStats, SigmaMultiplier(DynStackStats), serialExecution)
            If selectedID = -1 Then
                Call ThrowStackCreationException("Possibly an endless loop in a random selection from an array of possible fighters", _
                                                 GenSettings, DynStackStats)
            End If
            SelectedFighter = New AllDataStructues.Stack.UnitInfo(AllUnits(selectedID).unitID, AllUnits(selectedID).level, Nothing, Me)
            Call ChangeLimit(SelectedFighter.unit, DynStackStats, FreeMeleeSlots, LogID)
        Else
            SelectedFighter = Nothing
        End If
        Return SelectedFighter
    End Function
    Private Function SelectPossibleFighter(ByRef skipMaxGiantsFilter As Boolean, _
                                           ByRef skipRangeFilter As Boolean, _
                                           ByRef fighterID As Integer, _
                                           ByRef DynStackStats As AllDataStructues.DesiredStats, _
                                           ByRef FreeMeleeSlots As Integer, _
                                           ByRef SelectedLeader As AllDataStructues.Stack.UnitInfo, _
                                           ByRef SelectedFighters As List(Of AllDataStructues.Stack.UnitInfo), _
                                           ByRef BaseStackSize As Integer, _
                                           ByRef MapLordsRaces As List(Of Integer)) As Boolean
        If Not comm.IsAppropriateFighter(AllUnits(fighterID)) Then Return False

        If AllUnits(fighterID).fromRaceBranch AndAlso MapLordsRaces.Contains(AllUnits(fighterID).race) Then Return False

        If comm.SoleUnits.ContainsKey(AllUnits(fighterID).unitID) Then
            Dim sole As List(Of String) = comm.SoleUnits.Item(AllUnits(fighterID).unitID)
            If Not IsNothing(SelectedLeader) AndAlso sole.Contains(SelectedLeader.unit.unitID) Then Return False
            For Each u As AllDataStructues.Stack.UnitInfo In SelectedFighters
                If sole.Contains(u.unit.unitID) Then Return False
            Next u
        End If

        If Not settings.ignoreUnitRace Then
            If Not DynStackStats.Race.Contains(AllUnits(fighterID).race) Then Return False
        End If

        If comm.BigStackUnits.ContainsKey(AllUnits(fighterID).unitID) _
        AndAlso BaseStackSize < comm.BigStackUnits.Item(AllUnits(fighterID).unitID) Then Return False

        'Dim mult As Double
        'If AllFighters(fighterID).small Then
        '    mult = 1
        'Else
        '    mult = 2
        'End If
        'If AllFighters(fighterID).EXPkilled > mult * DynStackStats.ExpStackKilled / DynStackStats.StackSize Then Return False
        If DynStackStats.ExpStackKilled <= 0 Then Return False
        If AllUnits(fighterID).EXPkilled > 1.15 * DynStackStats.ExpStackKilled + 15 Then Return False
        If Not AllUnits(fighterID).small Then
            If DynStackStats.MaxGiants = 0 And Not skipMaxGiantsFilter Then Return False
            If DynStackStats.StackSize < 2 Then Return False
            If FreeMeleeSlots = 0 Then Return False
        End If

        If AllUnits(fighterID).reach = GenDefaultValues.UnitAttackReach.melee Then
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

    Private Function SetUnitPosition(ByRef i As Integer, ByRef units() As AllDataStructues.Stack.UnitInfo, _
                                     ByRef FRowSlots As Integer, ByRef SRowSlots As Integer, _
                                     ByRef AnySlot As Boolean, ByRef result As AllDataStructues.Stack) As Boolean
        Dim placed As Boolean = False
        Dim n1 As Integer = rndgen.RndPos(FRowSlots, True)
        Dim n2 As Integer = rndgen.RndPos(SRowSlots, True)

        Dim t As Integer
        Dim m As Integer = 0
        For k As Integer = 0 To UBound(firstrow) Step 1
            If (Not units(i).unit.small Or units(i).unit.reach = GenDefaultValues.UnitAttackReach.melee) And Not AnySlot Then
                If IsNothing(result.units(firstrow(k))) Then
                    m += 1
                    If m = n1 Then
                        Call SetUnitPos(result, firstrow(k), units(i))
                        FRowSlots -= 1
                        If Not units(i).unit.small Then
                            result.units(busytransfer(firstrow(k))) = AllDataStructues.Stack.UnitInfo.CreateEmpty
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
                    If IsNothing(result.units(t)) Then
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
                If IsNothing(result.units(secondrow(k))) Then
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
    Private Sub SetUnitPos(ByRef result As AllDataStructues.Stack, ByRef pos As Integer, _
                           ByRef unit As AllDataStructues.Stack.UnitInfo)
        result.units(pos) = AllDataStructues.Stack.UnitInfo.Copy(unit)
    End Sub

    Private Sub ChangeLimit(ByRef unit As AllDataStructues.Unit, _
                            ByRef DynStackStats As AllDataStructues.DesiredStats,
                            ByRef FreeMeleeSlots As Integer, ByRef LogID As Integer)
        DynStackStats.ExpStackKilled -= unit.EXPkilled

        If unit.small And DynStackStats.StackSize > 1 Then
            DynStackStats.ExpBarAverage = Math.Max(CInt((DynStackStats.StackSize * DynStackStats.ExpBarAverage - unit.EXPnext) / CDbl(DynStackStats.StackSize - 1)), 10)
        ElseIf Not unit.small And DynStackStats.StackSize > 2 Then
            DynStackStats.ExpBarAverage = Math.Max(CInt((DynStackStats.StackSize * DynStackStats.ExpBarAverage - unit.EXPnext) / CDbl(DynStackStats.StackSize - 2)), 10)
        End If

        If Not unit.small Then
            DynStackStats.MaxGiants -= 1
            DynStackStats.StackSize -= 2
            FreeMeleeSlots -= 1
        ElseIf unit.reach = GenDefaultValues.UnitAttackReach.melee Then
            DynStackStats.StackSize -= 1
            DynStackStats.MeleeCount = Math.Max(0, DynStackStats.MeleeCount - 1)
            FreeMeleeSlots = Math.Max(0, FreeMeleeSlots - 1)
        Else
            DynStackStats.StackSize -= 1
        End If
        DynStackStats.MeleeCount = Math.Min(DynStackStats.MeleeCount, FreeMeleeSlots)

        Call AddToLog(LogID, "Unit added: " & unit.name & " id: " & unit.unitID)
        Call AddToLog(LogID, DynStackStats)
    End Sub
#End Region

End Class

Public Class RndValueGen
    Private betTick, lastRAM As Double
    Private tempPat, delimiterBias As Integer
    Private fastModeSeed, fastModeTicks As Long
    Private Const seedMaxVal As Long = Integer.MaxValue - 10

    Public Sub New(Optional ByVal seed As Integer = -1)
        For i As Integer = 0 To 10 Step 1
            Call RndDbl()
        Next i
        If seed = -1 Then
            fastModeSeed = Math.Min(CLng(Math.Abs(Environment.TickCount)) + _
                                    CLng((Threading.Thread.CurrentThread.ManagedThreadId + 17) ^ 3), seedMaxVal)
        Else
            fastModeSeed = Math.Min(Math.Abs(seed), seedMaxVal)
        End If
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
    Public Function PRandTestOnly(ByVal lower As Double, ByVal upper As Double) As Double
        Dim value(Environment.ProcessorCount - 1) As Double
        Dim n As Integer = CInt(Math.Round(Rand(0, Environment.ProcessorCount - 1), 0))
        Parallel.For(0, Environment.ProcessorCount, _
         Sub(i As Integer)
             value(i) = Rand(lower, upper)
         End Sub)
        Return value(n)
    End Function
    Public Function PRand(ByVal lower As Double, ByVal upper As Double) As Double
        Dim value(CInt(0.5 * Environment.ProcessorCount)) As Double
        Parallel.For(0, value.Length, _
         Sub(i As Integer)
             value(i) = Rand(lower, upper)
         End Sub)
        Return value(UBound(value))
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

    '''<summary>Returns random value with uniform distribution from min to max.</summary>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RndInt(ByRef min As Integer, ByRef max As Integer, ByRef serial As Boolean) As Integer
        Return min - 1 + RndPos(max - min + 1, serial)
    End Function

    '''<summary>Returns random value with uniform distribution from min to max.
    ''' Runs approximately 100 times faster than regular.
    ''' Use in the serial execution mode only.</summary>
    Public Function RndDblFast(ByRef min As Double, ByRef max As Double) As Double
        Return min + (max - min) * RndIntFast(seedMaxVal) / seedMaxVal
    End Function
    '''<summary>Returns random value with uniform distribution from min to max.
    ''' Runs approximately 100 times faster than regular.
    ''' Use in the serial execution mode only.</summary>
    Public Function RndIntFast(ByRef min As Integer, ByRef max As Integer) As Integer
        Return min + RndIntFast(max - min)
    End Function
    Private Function RndIntFast(ByRef max As Integer) As Integer

        Dim m As Long
        Dim b As Long = CLng(max)

        Dim tid As Long = Threading.Thread.CurrentThread.ManagedThreadId

        Dim c1 As Long = fastModeTicks + fastModeSeed + 11
        Dim c2 As Long = fastModeTicks + b + tid
        Dim c3 As Long = fastModeTicks + 112
        Dim c4 As Long = b + b + 1 + tid * tid
        Dim c5 As Long = b + 1021

        Dim d1 As Long = fastModeSeed + b + 1
        Dim d2 As Long = fastModeTicks + 3

        Dim e1 As Long = CLng(0.5 * fastModeSeed)

        If c1 > seedMaxVal Then c1 -= seedMaxVal
        If c1 > seedMaxVal Then c1 -= seedMaxVal
        If c2 > seedMaxVal Then c2 -= seedMaxVal
        If c3 > seedMaxVal Then c3 -= seedMaxVal
        If c4 > seedMaxVal Then c4 -= seedMaxVal
        If c5 > seedMaxVal Then c5 -= seedMaxVal
        If d1 > seedMaxVal Then d1 -= seedMaxVal
        If d2 > seedMaxVal Then d2 -= seedMaxVal
        If e1 > seedMaxVal Then e1 -= seedMaxVal

        Dim s1, s2 As Long
        s1 = c1 * c2
        If s1 > seedMaxVal Then s1 = s1 Mod seedMaxVal
        s1 *= c3
        If s1 > seedMaxVal Then s1 = s1 Mod seedMaxVal
        s1 *= c4
        If s1 > seedMaxVal Then s1 = s1 Mod seedMaxVal
        s1 *= c5
        If s1 > seedMaxVal Then s1 = s1 Mod seedMaxVal
        s2 = d1 * d2
        If s2 > seedMaxVal Then s2 = s2 Mod seedMaxVal

        m = s1 + s2 + e1

        fastModeSeed = (fastModeSeed + m + fastModeTicks) Mod seedMaxVal
        fastModeTicks += 1
        If fastModeTicks > seedMaxVal Then fastModeTicks -= seedMaxVal

        Return CInt(m Mod (max + 1))
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
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByRef mult() As Double, _
                                    ByVal BaseSmearing As Double, ByVal serial As Boolean, _
                                    Optional ByVal fastMode As Boolean = False) As Integer
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
                        Weight(i) *= Common.Gauss(Stats(j)(i), m * DesiredStats(j), smearing)
                    Next j
                End If
                WeightsSum += Weight(i)
            Next i
            If smearing > maxSmearing AndAlso WeightsSum = 0 Then
                If Not noValue Then
                    Dim minAbs(UBound(Stats)) As Double
                    For j As Integer = 0 To UBound(Stats) Step 1
                        minAbs(j) = Double.MaxValue
                    Next j
                    For Each i As Integer In IDs
                        If Not IsNothing(mult) Then
                            m = mult(i)
                        Else
                            m = 1
                        End If
                        Dim invM As Double = 1 / m
                        For j As Integer = 0 To UBound(Stats) Step 1
                            minAbs(j) = Math.Min(minAbs(j), Math.Abs(DesiredStats(j) - Stats(j)(i) * invM))
                        Next j
                    Next i

                    WeightsSum = 0
                    Dim s, d As Double
                    For Each i As Integer In IDs
                        Weight(i) = 1
                        If Not IsNothing(mult) Then
                            m = mult(i)
                        Else
                            m = 1
                        End If
                        For j As Integer = 0 To UBound(Stats) Step 1
                            d = m * DesiredStats(j)
                            s = Stats(j)(i)
                            If s < d Then
                                s += minAbs(j) * m
                            Else
                                s -= minAbs(j) * m
                            End If
                            Weight(i) *= Common.Gauss(s, d, BaseSmearing)
                        Next j
                        WeightsSum += Weight(i)
                    Next i
                End If
                If WeightsSum = 0 Then
                    For Each i As Integer In IDs
                        Weight(i) = 1
                    Next i
                End If
                Exit Do
            End If
        Loop
        Return RandomSelection(IDs, Weight, serial, fastMode)
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
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByVal BaseSmearing As Double, _
                                    ByVal serial As Boolean, Optional ByVal fastMode As Boolean = False) As Integer
        Return RandomSelection(IDs, Stats, DesiredStats, Nothing, BaseSmearing, serial, fastMode)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка, считая, что у всех записей будет одинаковый стат. вес</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор</param>
    ''' <param name="serial">True, if use in serial code</param>
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), _
                                    ByVal serial As Boolean, Optional ByVal fastMode As Boolean = False) As Integer
        Return RandomSelection(IDs, Nothing, Nothing, 0, serial, fastMode)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор.
    ''' Если массив Weight не инициализирован, то у всех записей будет одинаковый стат. вес</param>
    ''' <param name="Weight">Вероятность выбрать запись прямо пропорциональна величине стат. веса. Сумма весов может быть не равна единице</param>
    ''' <param name="serial">True, if use in serial code</param>
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Weight() As Double, _
                                    ByVal serial As Boolean, Optional ByVal fastMode As Boolean = False) As Integer
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
        Dim R As Double
        If Not fastMode Then
            R = Rand(0, WeightsSum, serial)
        Else
            R = RndDblFast(0, WeightsSum)
        End If
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

    ''' <summary>Перемешает массив</summary>
    ''' <param name="v">Массив</param>
    Public Sub Shuffle(ByRef v() As Integer)
        'Dim rnd As New RndValueGen(seed)
        If UBound(v) < 1 Then Exit Sub
        Dim t, m As Integer
        Dim u As Integer = UBound(v)
        For i As Integer = 0 To 3 * UBound(v) Step 1
            m = RndIntFast(0, u - 1)
            t = v(m)
            v(m) = v(u)
            v(u) = t
        Next i
    End Sub

End Class

Public Class Common

    Public rndgen As New RndValueGen
    ''' <summary>Список исключаемых объектов</summary>
    Public excludedObjects As New List(Of String)
    ''' <summary>Список предметов, которые нельзя перегенерировать</summary>
    Public preservedItems As New List(Of String)
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
    Friend defValues As GenDefaultValues
    Protected Friend ReadingLog As New Log(Me)

    Friend ConsumableItemsTypes, NonconsumableItemsTypes, JewelItemsTypes As New List(Of Integer)
    Friend ItemTypesLists() As List(Of Integer)

    Protected Friend Delegate Sub RefreshExcluded()

    Protected Friend onExcludedListChanged As RefreshExcluded = Nothing

    ''' <param name="modName">Название мода, на котором происходит генерация.
    ''' Ваианты можно получить из GenDefaultValues.GetSupportedMods</param>
    Public Sub New(ByVal modName As String)
        Call ReadingLog.Enable()

        defValues = New GenDefaultValues(ReadingLog, modName)

        'Dim splitedFields() As String = TxtSplit(defValues.StackStatsFields)
        'ReDim StatFields(CInt(splitedFields.Length / 2 - 1))

        Dim racesList As String = ""
        Dim splitedRace() As String = defValues.Races.Replace(Chr(10), Chr(13)).Split(Chr(13))
        For i As Integer = 0 To UBound(splitedRace) Step 1
            If splitedRace(i).Length > 1 AndAlso Not splitedRace(i).Substring(0, 1) = "#" Then
                racesList &= splitedRace(i) & vbNewLine
            End If
        Next i
        'Dim valConv As New ValueConverter
        'Dim k As Integer
        'For i As Integer = 0 To UBound(splitedFields) Step 2
        '    k = CInt(i / 2)
        '    StatFields(k).description = splitedFields(i)
        '    StatFields(k).name = splitedFields(i + 1)
        '    StatFields(k).description = StatFields(k).description.Replace("$jm$", defValues.JewelItemsCostDevider.ToString)
        '    StatFields(k).description = StatFields(k).description.Replace("$gm$", defValues.giantUnitsExpMultiplier.ToString)
        '    StatFields(k).description = StatFields(k).description.Replace("$ri$", racesList)
        '    StatFields(k).description = StatFields(k).description.Replace("$newline$", vbNewLine)
        'Next i

        Dim lords() As String = TxtSplit(defValues.Lords)
        For i As Integer = 0 To UBound(lords) Step 1
            Dim s() As String = lords(i).Split(CChar(" "))
            LordsRace.Add(s(0).ToUpper, RaceIdentifierToSubrace(s(1)))
        Next i

        ConsumableItemsTypes.AddRange(New Integer() {GenDefaultValues.ItemTypes.elixir, _
                                                     GenDefaultValues.ItemTypes.healing_elixir, _
                                                     GenDefaultValues.ItemTypes.ressurection_elixir, _
                                                     GenDefaultValues.ItemTypes.permanent_elixir, _
                                                     GenDefaultValues.ItemTypes.scroll, _
                                                     GenDefaultValues.ItemTypes.sphere, _
                                                     GenDefaultValues.ItemTypes.talisman})
        NonconsumableItemsTypes.AddRange(New Integer() {GenDefaultValues.ItemTypes.nonattack_artifact, _
                                                        GenDefaultValues.ItemTypes.relic, _
                                                        GenDefaultValues.ItemTypes.attack_artifact, _
                                                        GenDefaultValues.ItemTypes.banner, _
                                                        GenDefaultValues.ItemTypes.stuff, _
                                                        GenDefaultValues.ItemTypes.boots})
        JewelItemsTypes.AddRange(New Integer() {GenDefaultValues.ItemTypes.jewel})
        ItemTypesLists = {ConsumableItemsTypes, NonconsumableItemsTypes, JewelItemsTypes}

        Call defValues.ParseItemTypes(itemTypeID, itemType)

    End Sub

#Region "Object usage check"
    Friend Function IsAppropriateFighter(ByRef unit As AllDataStructues.Unit) As Boolean
        If unit.useState = GenDefaultValues.ExclusionState.unknown Then
            If IsExcluded(unit) Then Return False
        Else
            If unit.useState = GenDefaultValues.ExclusionState.excluded Then Return False
        End If
        If unit.unitBranch > GenDefaultValues.UnitClass.special Then Return False
        Return True
    End Function
    Friend Function IsAppropriateLeader(ByRef unit As AllDataStructues.Unit) As Boolean
        If unit.useState = GenDefaultValues.ExclusionState.unknown Then
            If IsExcluded(unit) Then Return False
        Else
            If unit.useState = GenDefaultValues.ExclusionState.excluded Then Return False
        End If
        If Not unit.unitBranch = GenDefaultValues.UnitClass.leader Then Return False
        Return True
    End Function
    Friend Function IsAppropriateItem(ByRef item As AllDataStructues.Item) As Boolean
        If item.useState = GenDefaultValues.ExclusionState.unknown Then
            If IsExcluded(item) Then Return False
        Else
            If item.useState = GenDefaultValues.ExclusionState.excluded Then Return False
        End If
        If item.type = GenDefaultValues.ItemTypes.jewel Then
            If item.itemCost.Gold = 0 Then Return False
        Else
            If item.itemCostSum = 0 Then Return False
        End If
        Return True
    End Function
    Protected Friend Function IsExcluded(ByRef item As AllDataStructues.Item) As Boolean
        If excludedObjects.Contains(item.itemID.ToUpper) Then Return True
        If IsExcluded(item.type) Then Return True
        Return False
    End Function
    Protected Friend Function IsExcluded(ByRef unit As AllDataStructues.Unit) As Boolean
        Return excludedObjects.Contains(unit.unitID.ToUpper)
    End Function
    Protected Friend Function IsExcluded(ByRef spell As AllDataStructues.Spell) As Boolean
        Return excludedObjects.Contains(spell.spellID.ToUpper)
    End Function
    Protected Friend Function IsExcluded(ByRef iType As GenDefaultValues.ItemTypes) As Boolean
        Return excludedObjects.Contains(itemType.Item(iType).ToUpper)
    End Function

    Friend Function IsPreserved(ByRef item As AllDataStructues.Item) As Boolean
        If item.type = GenDefaultValues.ItemTypes.special Then Return True
        If preservedItems.Contains(item.itemID.ToUpper) Then Return True
        If preservedItems.Contains(itemType.Item(item.type).ToUpper) Then Return True
        Return False
    End Function
    Friend Function IsPreserved(ByRef unit As AllDataStructues.Unit) As Boolean
        If preservedItems.Contains(unit.unitID.ToUpper) Then Return True
        Return False
    End Function
    Friend Function IsPreserved(ByRef spell As AllDataStructues.Spell) As Boolean
        If preservedItems.Contains(spell.spellID.ToUpper) Then Return True
        Return False
    End Function
#End Region

    ''' <summary>Передаст в лог содержимое excludedObjects, customRace, objectRace, LootItemChanceMultiplier, SoleUnits</summary>
    ''' <param name="log">Сюда будем писать данные</param>
    ''' <param name="rStack">Инициализированный класс</param>
    Public Sub PrintResourcesToLog(ByRef log As Log, ByRef rStack As RandStack)
        If Not log.IsEnabled Then Exit Sub

        log.Add(GenDefaultValues.PrintVersion)

        Dim Races As New Dictionary(Of String, String)
        Dim t() As String = TxtSplit(defValues.Races)
        For Each s As String In t
            Dim r() As String = s.Split(CChar(" "))
            For i As Integer = 0 To UBound(r) Step 1
                Races.Add(r(i).ToUpper, r(0))
            Next i
        Next s

        Dim result, name As String
        result = vbNewLine & "----Excluded objects list----"
        For Each item As String In excludedObjects
            name = ""
            If name = "" Then
                Dim f As AllDataStructues.Unit = rStack.FindUnitStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
            If name = "" Then
                Dim f As AllDataStructues.Item = rStack.FindItemStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
            If name = "" Then
                Dim f As AllDataStructues.Spell = rStack.FindSpellStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
            If name = "" AndAlso rStack.comm.itemType.ContainsValue(item.ToUpper) Then name = "item type"
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & name
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Custom units races list----"
        For Each item As String In customRace.Keys
            name = ""
            If name = "" Then
                Dim f As AllDataStructues.Unit = rStack.FindUnitStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
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
            name = ""
            If name = "" Then
                Dim f As AllDataStructues.Item = rStack.FindItemStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & LootItemChanceMultiplier.Item(item) & " - " & name
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Sole units list----"
        For Each item As String In SoleUnits.Keys
            name = ""
            If name = "" Then
                Dim f As AllDataStructues.Unit = rStack.FindUnitStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
            If name = "" Then name = "???"
            result &= vbNewLine & item & " - " & name & " // "
            Dim s As String = ""
            For Each u As String In SoleUnits.Item(item)
                name = ""
                If name = "" Then
                    Dim f As AllDataStructues.Unit = rStack.FindUnitStats(u)
                    If Not IsNothing(f) Then name = f.name
                End If
                If name = "" Then name = "???"
                If Not s = "" Then s &= " # "
                s &= u & " - " & name
            Next u
            result &= s
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Big stack units list----"
        For Each item As String In BigStackUnits.Keys
            name = ""
            If name = "" Then
                Dim f As AllDataStructues.Unit = rStack.FindUnitStats(item)
                If Not IsNothing(f) Then name = f.name
            End If
            If name = "" Then name = "I don't know what is that"
            result &= vbNewLine & item & " - " & name & " - " & BigStackUnits.Item(item)
        Next item
        Call log.Add(result)

        result = vbNewLine & "----Reading files log----" & vbNewLine & ReadingLog.PrintAll
        Call log.Add(result)

    End Sub

    ''' <summary>Возвращает ID расы, соответствующее файлам игры. Если не найдет, то вернет -1</summary>
    ''' <param name="ID">Идентификатор расы (файл races.txt)</param>
    Public Function RaceIdentifierToSubrace(ByVal ID As String, Optional ByVal ThrowExceptionIfUnknownID As Boolean = True) As Integer
        Dim uID As String = ID.ToUpper
        If defValues.linked_Races.ContainsKey(uID) Then
            Return defValues.linked_Races.Item(uID)
        Else
            If ThrowExceptionIfUnknownID Then Throw New Exception("Неизвестный идентификатор расы:" & ID)
            Return -1
        End If
    End Function
    ''' <summary>Возвращает ID расы, соответствующее файлам игры</summary>
    ''' <param name="ID">Идентификатор расы (файл races.txt)</param>
    Public Function RaceIdentifierToSubrace(ByVal ID As Integer) As Integer
        Return RaceIdentifierToSubrace(ID.ToString)
    End Function

#Region "TxtSplit"
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    Public Function TxtSplit(ByVal TXT As String) As String()
        Return ValueConverter.TxtSplit(TXT)
    End Function
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    ''' <param name="transferChar">Если строка заканчивается этой подстрокой, то подстрока удаляется, а текущая строка объединяется со следующей</param>
    Public Function TxtSplit(ByVal TXT As String, ByRef transferChar As String) As String()
        Return ValueConverter.TxtSplit(TXT, transferChar)
    End Function
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    Public Function TxtSplit(ByVal TXT() As String) As String()
        Return ValueConverter.TxtSplit(TXT)
    End Function
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    ''' <param name="transferChar">Если строка заканчивается этой подстрокой, то подстрока удаляется, а текущая строка объединяется со следующей</param>
    Public Function TxtSplit(ByVal TXT() As String, ByRef transferChar As String) As String()
        Return ValueConverter.TxtSplit(TXT, transferChar)
    End Function
#End Region

#Region "RandomSelection"
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
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByRef mult() As Double, _
                                    ByVal BaseSmearing As Double, ByVal serial As Boolean, _
                                    Optional ByVal fastMode As Boolean = False) As Integer
        Return rndgen.RandomSelection(IDs, Stats, DesiredStats, mult, BaseSmearing, serial, fastMode)
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
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByVal BaseSmearing As Double, _
                                    ByVal serial As Boolean, Optional ByVal fastMode As Boolean = False) As Integer
        Return rndgen.RandomSelection(IDs, Stats, DesiredStats, BaseSmearing, serial, fastMode)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка, считая, что у всех записей будет одинаковый стат. вес</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор</param>
    ''' <param name="serial">True, if use in serial code</param>
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), _
                                    ByVal serial As Boolean, Optional ByVal fastMode As Boolean = False) As Integer
        Return rndgen.RandomSelection(IDs, serial, fastMode)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор.
    ''' Если массив Weight не инициализирован, то у всех записей будет одинаковый стат. вес</param>
    ''' <param name="Weight">Вероятность выбрать запись прямо пропорциональна величине стат. веса. Сумма весов может быть не равна единице</param>
    ''' <param name="serial">True, if use in serial code</param>
    ''' <param name="fastMode">Generate random value faster. Don't recomended for parallel execution</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Weight() As Double, _
                                    ByVal serial As Boolean, Optional ByVal fastMode As Boolean = False) As Integer
        Return rndgen.RandomSelection(IDs, Weight, serial, fastMode)
    End Function
#End Region

    ''' <summary>e^(-0.5 * ((X - avX) / (sigma * avX)) ^ 2)</summary>
    Public Shared Function Gauss(ByVal X As Double, ByVal avX As Double, ByVal sigma As Double) As Double
        Return Math.Exp(-0.5 * ((X - avX) / (sigma * avX)) ^ 2)
    End Function

#Region "Reader"
    Private Enum ReadMode
        ExcludedObjects = 1
        CustomUnitRace = 2
        CustomBuildingRace = 3
        PlateauConstructionDescription = 4
        LootItemChanceMultiplier = 5
        SoleUnits = 6
        BigStackUnits = 7
        ReadPreservedObjects = 8
    End Enum
    ''' <summary>Читает список юнитов, предметов и заклинаний, которые не должен использовать генератор</summary>
    Public Sub ReadExcludedObjectsList()
        Dim s() As String = SettingsFileSplit(defValues.ExcludedIDs)
        Call ReadFile(ReadMode.ExcludedObjects, s)
        If Not IsNothing(onExcludedListChanged) Then Call onExcludedListChanged()
    End Sub
    ''' <summary>Читает список лорных юнитов, предметов и заклинаний, которые не должен использовать генератор</summary>
    Public Sub ReadExcludedLoreObjectsList()
        Dim s() As String = SettingsFileSplit(defValues.ExcludedIDs_ModLore)
        Call ReadFile(ReadMode.ExcludedObjects, s)
        If Not IsNothing(onExcludedListChanged) Then Call onExcludedListChanged()
    End Sub
    ''' <summary>Читает список юнитов, предметов и заклинаний, которые не должен использовать генератор</summary>
    ''' <param name="ExcludeLists">Файлы со списками исключенных объектов. Записи в них могут повторяться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Не воспринимает ключевые слова</param>
    Public Sub ReadExcludedObjectsList(ByRef ExcludeLists As List(Of String))
        Call ReadFile(ReadMode.ExcludedObjects, ExcludeLists)
        If Not IsNothing(onExcludedListChanged) Then Call onExcludedListChanged()
    End Sub
    ''' <summary>Читает множители шанса выпадения для отдельных предметов</summary>
    Protected Friend Sub ReadLootItemChanceMultiplier()
        Dim s() As String = SettingsFileSplit(defValues.LootItemChanceMultiplier)
        Call ReadFile(ReadMode.LootItemChanceMultiplier, s)
    End Sub
    ''' <summary>Читает множители шанса выпадения для отдельных предметов</summary>
    ''' <param name="MultipliersList">Множители шанса появления определенных предметов.
    ''' Допускается передача неинициализитрованного массива.
    ''' Не воспринимает ключевые слова</param>
    Protected Friend Sub ReadLootItemChanceMultiplier(ByRef MultipliersList As List(Of String))
        Call ReadFile(ReadMode.LootItemChanceMultiplier, MultipliersList)
    End Sub
    ''' <summary>Читает список, переопределяющий расы нужных юнитов</summary>
    Protected Friend Sub ReadCustomUnitRace()
        Dim s() As String = SettingsFileSplit(defValues.UnitRace)
        Call ReadFile(ReadMode.CustomUnitRace, s)
    End Sub
    ''' <summary>Читает список, переопределяющий расы нужных юнитов</summary>
    ''' <param name="CustomUnitRace">Список рас юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного списка.
    ''' Не воспринимает ключевые слова</param>
    Protected Friend Sub ReadCustomUnitRace(ByRef CustomUnitRace As List(Of String))
        Call ReadFile(ReadMode.CustomUnitRace, CustomUnitRace)
    End Sub
    ''' <summary>Читает список юнитов, которые должны находиться в отряде в единственном экземпляре</summary>
    Protected Friend Sub ReadSoleUnits()
        Dim s() As String = SettingsFileSplit(defValues.SingleUnits)
        Call ReadFile(ReadMode.SoleUnits, s)
    End Sub
    ''' <summary>Читает список юнитов, которые должны находиться в отряде в единственном экземпляре</summary>
    ''' <param name="SoleUnitsList">Cписок юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного списка.
    ''' Не воспринимает ключевые слова</param>
    Protected Friend Sub ReadSoleUnits(ByRef SoleUnitsList As List(Of String))
        Call ReadFile(ReadMode.SoleUnits, SoleUnitsList)
    End Sub
    ''' <summary>Читает список юнитов, которые могут находиться в отряде начиная с заданного количества слотов</summary>
    Protected Friend Sub ReadBigStackUnits()
        Dim s() As String = SettingsFileSplit(defValues.BigStackUnits)
        Call ReadFile(ReadMode.BigStackUnits, s)
    End Sub
    ''' <summary>Читает список юнитов, которые могут находиться в отряде начиная с заданного количества слотов</summary>
    ''' <param name="BigStackUnitsList">Список юнитов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться.
    ''' Допускается передача неинициализитрованного списка.
    ''' Не воспринимает ключевые слова</param>
    Protected Friend Sub ReadBigStackUnits(ByRef BigStackUnitsList As List(Of String))
        Call ReadFile(ReadMode.BigStackUnits, BigStackUnitsList)
    End Sub
    ''' <summary>Читает список, определяющий принадлежность непроходимых объектов</summary>
    Protected Friend Sub ReadCustomBuildingRace()
        Dim s() As String = SettingsFileSplit(defValues.MapObjectRace)
        Call ReadFile(ReadMode.CustomBuildingRace, s)
    End Sub
    ''' <summary>Читает список, определяющий принадлежность непроходимых объектов</summary>
    ''' <param name="CustomBuildingRace">Список рас и положений зданий. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного списка (будет прочтен дефолтный).
    ''' Не воспринимает ключевые слова</param>
    Protected Friend Sub ReadCustomBuildingRace(ByRef CustomBuildingRace As List(Of String))
        Call ReadFile(ReadMode.CustomBuildingRace, CustomBuildingRace)
    End Sub
    ''' <summary>Читает описание того, как цеплять друг к другу "Плато" и "Водопады"</summary>
    Protected Friend Sub ReadPlateauConstructionDescription()
        Dim s() As String = SettingsFileSplit(defValues.PlateauConstructor)
        Call ReadFile(ReadMode.PlateauConstructionDescription, s)
    End Sub
    ''' <summary>Читает описание того, как цеплять друг к другу "Плато" и "Водопады"</summary>
    ''' <param name="PlateauConstructionDescription">Описания.
    ''' Допускается передача неинициализитрованного списка (будет прочтен дефолтный).
    ''' Не воспринимает ключевые слова</param>
    Protected Friend Sub ReadPlateauConstructionDescription(ByRef PlateauConstructionDescription As List(Of String))
        Call ReadFile(ReadMode.PlateauConstructionDescription, PlateauConstructionDescription)
    End Sub
    ''' <summary>Читает список предметов, юнитов и заклинаний, которые генератор должен оставлять на месте</summary>
    Public Sub ReadPreservedObjects()
        Dim s() As String = SettingsFileSplit(defValues.PreservedItems)
        Call ReadFile(ReadMode.ReadPreservedObjects, s)
    End Sub
    ''' <summary>Добавляет лорных юнитов а список объектов, которые генератор должен оставлять на месте</summary>
    Protected Friend Sub ReadLoreUnitsToPreservedObjects()
        Dim s() As String = SettingsFileSplit(defValues.ExcludedIDs_ModLore)
        Call ReadFile(ReadMode.ReadPreservedObjects, s)
    End Sub
    ''' <summary>Читает список предметов, юнитов и заклинаний, которые генератор должен оставлять на месте</summary>
    ''' <param name="PreservedLists">Список ID. Не воспринимает ключевые слова</param>
    Public Sub ReadPreservedObjects(ByRef PreservedLists As List(Of String))
        Call ReadFile(ReadMode.ReadPreservedObjects, PreservedLists)
    End Sub
    Private Function SettingsFileSplit(ByRef fileContent As String) As String()
        If IsNothing(fileContent) Then Return Nothing
        If fileContent = "" Then Return Nothing
        Return TxtSplit(fileContent)
    End Function
    Private Sub ReadFile(ByRef mode As ReadMode, ByRef t As List(Of String))
        If IsNothing(t) Then Exit Sub
        Dim s() As String = t.ToArray
        Call ReadFile(mode, s)
    End Sub
    Private Sub ReadFile(ByRef mode As ReadMode, ByRef s() As String)
        If IsNothing(s) Then Exit Sub
        Dim srow(), r As String
        For j As Integer = 0 To UBound(s) Step 1
            srow = s(j).Split(CChar(" "))

            If mode = ReadMode.ExcludedObjects Then
                If Not excludedObjects.Contains(srow(0).ToUpper) Then excludedObjects.Add(srow(0).ToUpper)
            ElseIf mode = ReadMode.CustomUnitRace Then
                If srow.Length > 2 Then
                    If customRace.ContainsKey(srow(0).ToUpper) Then customRace.Remove(srow(0).ToUpper)
                    customRace.Add(srow(0).ToUpper, srow(2).ToUpper)
                End If
            ElseIf mode = ReadMode.CustomBuildingRace Then
                If srow.Length > 1 Then
                    If objectRace.ContainsKey(srow(0).ToUpper) Then objectRace.Remove(srow(0).ToUpper)
                    objectRace.Add(srow(0).ToUpper, New DecorationPlacingProperties(srow, Me))
                End If
            ElseIf mode = ReadMode.PlateauConstructionDescription Then
                If srow.Length > 1 Then
                    If PlateauConstruction.ContainsKey(srow(0).ToUpper) Then PlateauConstruction.Remove(srow(0).ToUpper)
                    r = ""
                    For i As Integer = 1 To UBound(srow) Step 1
                        If i > 1 Then r &= " "
                        r &= srow(i)
                    Next i
                    PlateauConstruction.Add(srow(0).ToUpper, r.ToUpper)
                End If
            ElseIf mode = ReadMode.LootItemChanceMultiplier Then
                If srow.Length > 1 Then
                    If LootItemChanceMultiplier.ContainsKey(srow(0).ToUpper) Then LootItemChanceMultiplier.Remove(srow(0).ToUpper)
                    LootItemChanceMultiplier.Add(srow(0).ToUpper, ValueConverter.StrToDbl(srow(1)))
                End If
            ElseIf mode = ReadMode.SoleUnits Then
                For i As Integer = 0 To UBound(srow) Step 1
                    If SoleUnits.ContainsKey(srow(i).ToUpper) Then SoleUnits.Remove(srow(i).ToUpper)
                    SoleUnits.Add(srow(i).ToUpper, New List(Of String))
                    For k As Integer = 0 To UBound(srow) Step 1
                        SoleUnits.Item(srow(i).ToUpper).Add(srow(k).ToUpper)
                    Next k
                Next i
            ElseIf mode = ReadMode.BigStackUnits Then
                If srow.Length > 1 Then
                    If BigStackUnits.ContainsKey(srow(0).ToUpper) Then BigStackUnits.Remove(srow(0).ToUpper)
                    BigStackUnits.Add(srow(0).ToUpper, CInt(srow(1)))
                End If
            ElseIf mode = ReadMode.ReadPreservedObjects Then
                If Not preservedItems.Contains(srow(0).ToUpper) Then preservedItems.Add(srow(0).ToUpper)
            Else
                Throw New Exception("Invalid read mode: " & mode.ToString)
            End If
        Next j
    End Sub
#End Region
#Region "RecursiveReadFile"
    ''' <summary>Прочитает файл и если встретит в нем ReadFromFile, прочитает и из указанного файла, 
    ''' разместив результат в том месте, где встретилось это ключевое слово</summary>
    ''' <param name="path">Путь к файлу</param>
    Public Shared Function RecursiveReadFile(ByVal path As String) As String()
        If Not IO.File.Exists(path) Then Throw New Exception("Could find file " & path)
        Dim content() As String = IO.File.ReadAllLines(path)
        Dim output() As String = RecursiveReadFile(content)
        Return output
    End Function
    ''' <summary>Прочитает файл и если встретит в нем ReadFromFile, прочитает и из указанного файла, 
    ''' разместив результат в том месте, где встретилось это ключевое слово.
    ''' Пропускает пустые строки</summary>
    ''' <param name="content">Содержимое файла</param>
    Public Shared Function RecursiveReadFile(ByVal content() As String) As String()
        Dim output(UBound(content)), r(), p As String
        Dim n As Integer = -1
        For i As Integer = 0 To UBound(content) Step 1
            If content(i).ToLower.StartsWith(My.Resources.template_read_from_file.ToLower) Then
                p = content(i).Substring(My.Resources.template_read_from_file.Length + 1)
                r = RecursiveReadFile(p)
                ReDim Preserve output(UBound(output) + r.Length)
                For k As Integer = 0 To UBound(r) Step 1
                    If Not r(k).Trim(CChar(" "), CChar(vbTab)) = "" Then
                        n += 1
                        output(n) = r(k)
                    End If
                Next k
                r = Nothing
            Else
                If Not content(i).Trim(CChar(" "), CChar(vbTab)) = "" Then
                    n += 1
                    output(n) = content(i)
                End If
            End If
        Next i
        If UBound(output) > n Then ReDim Preserve output(n)
        Return output
    End Function
#End Region

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
        If item.type = GenDefaultValues.ItemTypes.jewel Then
            Return item.itemCost / defValues.JewelItemsCostDevider
        Else
            Return item.itemCost / defValues.NonJewelItemsCostDevider
        End If
    End Function

End Class

Public MustInherit Class DecorationPlacingPropertiesFields
    ''' <summary>Допустимые расы</summary>
    Public race As New List(Of Integer)
    ''' <summary>Можно ли ставить на суше</summary>
    Public ground As Boolean
    ''' <summary>Можно ли ставить на воде</summary>
    Public water As Boolean
End Class

Public Class DecorationPlacingProperties
    Inherits DecorationPlacingPropertiesFields

    Public tags As New List(Of String)

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
                If racesRow(i).Contains("%") Then
                    If Not tags.Contains(racesRow(i).ToUpper) Then
                        tags.Add(racesRow(i).ToUpper)
                    End If
                Else
                    Try
                        id = comm.RaceIdentifierToSubrace(racesRow(i))
                        If Not race.Contains(id) Then race.Add(id)
                    Catch ex As Exception
                        Console.WriteLine(ex.Message)
                    End Try
                End If
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
        ''' <summary>Количество свободных ячеек под отряд. Есть 10% шанс на +1 слот и 10% на -1 слот</summary>
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
        ''' <summary>Суммарный оверлевел в отряде с учетом силы юнитов</summary>
        Dim WeightedOverlevel As Double
        ''' <summary>Если у отряда приказ отличный от "стоять",
        ''' лидер для создаваемого отряда будет строго соответствовать этой настройке</summary>
        Dim WaterOnly As Boolean

        ''' <summary>Выбранные юниты будут сохранены в отряде</summary>
        Dim preservedUnits As List(Of Stack.UnitInfo)

        ''' <summary>Не nothing только для торговцев предметами и магией, а также лагеря наемников.
        ''' Список идентификаторов содержимого лавки с предметами/заклинаниями/наемниками, 
        ''' либо параметра генерации (цена, тип или тип#цена для предмета, уровень для заклинания и 
        ''' планка опыта на 1 занимаемый слот для существа).
        ''' Типы предметов можно посмотреть в Items.txt или Common.itemType
        ''' Примеры:
        ''' Наемник - g000uu0001 200 700
        ''' Волшебник - g000ss0005 g000ss0006 1RT 2CF (T - может быть глобальным, F - не может. H,L,C,U,E - раса, R - случайная раса)
        ''' Торговец - g000ig0002 1200 750 attack_artifact sphere#300 6#400
        ''' </summary>
        Dim shopContent As List(Of String)

        ''' <summary>True, если отряд является внутренней охраной города</summary>
        Dim isInternalCityGuard As Boolean

        ''' <summary>Список модификаторов для лидера</summary>
        Dim LeaderModificators As List(Of String)
        ''' <summary>Список модификаторов, которые нужно распределить между юнитами</summary>
        Dim UnitsModificators As List(Of String)
        ''' <summary>Во сколько раз отряд стал сильнее из-за модификаторов</summary>
        Dim ModificatorsEffect As Double

        ''' <summary>
        ''' True, если в preservedUnits есть лидер
        ''' </summary>
        Public Function HasPreservedLeader() As Boolean
            If IsNothing(preservedUnits) Then Return False
            If preservedUnits.Count = 0 Then Return False
            For Each u As Stack.UnitInfo In preservedUnits
                If Not u.unit.unitID = GenDefaultValues.emptyItem Then
                    If u.unit.unitBranch = GenDefaultValues.UnitClass.leader Then Return True
                End If
            Next u
            Return False
        End Function
        ''' <summary>
        ''' Количество слотов под сохраненных юнитов
        ''' </summary>
        Public Function PreservedSlots() As Integer
            If IsNothing(preservedUnits) Then Return 0
            If preservedUnits.Count = 0 Then Return 0
            Dim n As Integer = 0
            For Each u As Stack.UnitInfo In preservedUnits
                If Not u.unit.unitID = GenDefaultValues.emptyItem Then
                    If u.unit.small Then
                        n += 1
                    Else
                        n += 2
                    End If
                End If
            Next u
            Return n
        End Function

        Public Shared Function Copy(ByVal v As DesiredStats) As DesiredStats
            Return New DesiredStats With {.ExpBarAverage = v.ExpBarAverage, _
                                          .ExpStackKilled = v.ExpStackKilled, _
                                          .MaxGiants = v.MaxGiants, _
                                          .MeleeCount = v.MeleeCount, _
                                          .Race = CopyList(v.Race), _
                                          .StackSize = v.StackSize, _
                                          .LootCost = v.LootCost, _
                                          .LocationName = v.LocationName, _
                                          .IGen = AllDataStructues.LootGenSettings.Copy(v.IGen), _
                                          .shopContent = CopyList(v.shopContent), _
                                          .isInternalCityGuard = v.isInternalCityGuard, _
                                          .WeightedOverlevel = v.WeightedOverlevel, _
                                          .preservedUnits = CopyList(v.preservedUnits), _
                                          .LeaderModificators = CopyList(v.LeaderModificators), _
                                          .UnitsModificators = CopyList(v.UnitsModificators), _
                                          .ModificatorsEffect = v.ModificatorsEffect}
        End Function
        ''' <param name="RaceNumberToRaceChar">Преобразует номер расы в ее текстовый идентификатор. Если передать Nothing, то будут печататься номера рас</param>
        Public Shared Function Print(ByVal v As DesiredStats, ByRef RaceNumberToRaceChar As Dictionary(Of Integer, String)) As String
            Dim s As String
            If IsNothing(v.shopContent) Then
                s = "ID" & vbTab & v.LocationName & vbNewLine & _
                    "AverageExpBar" & vbTab & v.ExpBarAverage & vbNewLine & _
                    "ExpStackKilled" & vbTab & v.ExpStackKilled & vbNewLine & _
                    "Race" & vbTab & PrintList(v.Race, "+", RaceNumberToRaceChar) & vbNewLine & _
                    "Preserved units" & vbNewLine & PrintList(v.preservedUnits, vbNewLine) & vbNewLine & _
                    "Has preserved leader" & vbNewLine & v.HasPreservedLeader & vbNewLine

                s &= "StackSize" & vbTab & v.StackSize & vbNewLine & _
                     "MaxGiants" & vbTab & v.MaxGiants & vbNewLine & _
                     "MeleeCount" & vbTab & v.MeleeCount & vbNewLine & _
                     "WeightedOverlevel" & vbTab & v.WeightedOverlevel & vbNewLine

                s &= "LootCost" & vbTab & v.LootCost & vbNewLine & _
                     "IsInternalCityGuard" & vbTab & v.isInternalCityGuard & vbNewLine & _
                     AllDataStructues.LootGenSettings.Print(v.IGen) & vbNewLine

                s &= "ModificatorsEffect" & vbTab & v.ModificatorsEffect & vbNewLine & _
                     "LeaderModificators" & vbTab & PrintList(v.LeaderModificators, "+") & vbNewLine & _
                     "UnitsModificators" & vbTab & PrintList(v.UnitsModificators, "+") & vbNewLine
            Else
                s = "ID" & vbTab & v.LocationName & vbNewLine & _
                    "ShopContent" & vbTab & PrintList(v.shopContent, "+") & vbNewLine
            End If
            Return s
        End Function
    End Structure

    Public Structure Stack
        ''' <summary>
        ''' Юниты отряда
        ''' </summary>
        Public units() As UnitInfo
        ''' <summary>В какой позиции находится лидер</summary>
        Public leaderPos As Integer
        ''' <summary>Предметы отряда. GxxxIGxxxx</summary>
        Public items As List(Of String)
        ''' <summary>Имя отряда</summary>
        Public name As String
        '''<summary>Приказ отряда</summary>
        Public order As String

        Public Class UnitInfo
            ''' <summary>Уровень юнита, находящегося в отряде</summary>
            Public level As Integer
            ''' <summary>Модификаторы юнита</summary>
            Public modificators As List(Of String)
            ''' <summary>Базовые статы юнита</summary>
            Public unit As Unit
            ''' <summary>
            ''' True, если юнит является сохраненным. У него не будет изменен уровень или модификаторы
            ''' </summary>
            Protected Friend isPreserved As Boolean

            Public Sub New(ByRef _id As String, ByRef _level As Integer, ByRef _modificators As List(Of String), _
                           ByRef RandStack As RandStack)
                If Not _id.ToUpper = GenDefaultValues.emptyItem.ToUpper Then
                    level = _level
                    unit = RandStack.FindUnitStats(_id)
                    If IsNothing(unit) Then Throw New Exception("Unknown unit id: " & _id)
                    If level < unit.level Then level = unit.level
                Else
                    level = 0
                    unit = New Unit With {.unitID = GenDefaultValues.emptyItem.ToUpper}
                End If
                If IsNothing(_modificators) Then
                    modificators = New List(Of String)
                Else
                    modificators = _modificators
                End If
            End Sub
            Private Sub New(ByRef _unit As Unit, ByRef _level As Integer, ByRef _modificators As List(Of String), _
                            ByRef _isPreserved As Boolean)
                level = _level
                If IsNothing(_modificators) Then
                    modificators = New List(Of String)
                Else
                    modificators = _modificators
                End If
                unit = _unit
                isPreserved = _isPreserved
            End Sub

            Public Shared Function CreateEmpty() As UnitInfo
                Return New UnitInfo(GenDefaultValues.emptyItem, 0, Nothing, Nothing)
            End Function
            Public Shared Function CreateArray(ByRef _unit() As String, ByRef _level() As Integer, ByRef _modificators() As List(Of String), _
                                               ByRef RandStack As RandStack) As UnitInfo()
                Dim r(UBound(_unit)) As UnitInfo
                Dim L() As Integer
                Dim m() As List(Of String)
                If IsNothing(_level) Then
                    ReDim L(UBound(r))
                Else
                    L = _level
                End If
                If IsNothing(_modificators) Then
                    ReDim m(UBound(r))
                Else
                    m = _modificators
                End If
                For i As Integer = 0 To UBound(_unit) Step 1
                    r(i) = New UnitInfo(_unit(i), L(i), m(i), RandStack)
                Next i
                Return r
            End Function

            Public Shared Function Copy(ByVal v As UnitInfo) As UnitInfo
                Return New UnitInfo(AllDataStructues.Unit.Copy(v.unit), _
                                    v.level, _
                                    CopyList(v.modificators), _
                                    v.isPreserved)
            End Function
            Public Shared Function Print(ByVal v As UnitInfo) As String
                Return "ID" & vbTab & v.unit.unitID & vbNewLine & _
                       "Level" & vbTab & v.level & vbNewLine & _
                       "Modificators" & vbTab & PrintList(v.modificators, "+")
            End Function
        End Class
    End Structure

    Public Class UnitBattleNumericValues
        ''' <summary>Очки здоровья</summary>
        Public hp As Integer
        ''' <summary>Броня</summary>
        Public armor As Integer
        ''' <summary>Урон</summary>
        Public damage As Integer
        ''' <summary>Восстановление здоровья</summary>
        Public heal As Integer
        ''' <summary>Точность основной атаки юнита</summary>
        Public accuracy As Integer
        ''' <summary>Инициатива юнита</summary>
        Public initiative As Integer

        Public Shared Function BaseBattleNumericStats(ByRef unit As Stack.UnitInfo) As UnitBattleNumericValues
            Return BaseBattleNumericStats(unit.unit, unit.level)
        End Function
        Public Shared Function BaseBattleNumericStats(ByRef unit As Unit, _
                                                      ByVal unitLevel As Integer) As UnitBattleNumericValues
            Dim base As New UnitBattleNumericValues

            base.accuracy = Math.Min(unit.accuracy + unit.GetAccuracyOverlevel(unitLevel), 100)
            base.armor = Math.Min(unit.armor + unit.GetArmorOverlevel(unitLevel), 90)
            If unit.damage > 0 Then base.damage = unit.damage + unit.GetDamageOverlevel(unitLevel)
            If unit.heal > 0 Then base.heal = unit.heal + unit.GetHealOverlevel(unitLevel)
            base.hp = unit.hp + unit.GetHPOverlevel(unitLevel)
            base.initiative = unit.initiative + unit.GetInitiativeOverlevel(unitLevel)

            If unit.heal > 0 And unit.damage > 0 Then Throw New Exception("unit.heal > 0 And unit.damage > 0")
            Return base
        End Function

        Public Shared Function UnitPower(ByRef stats As UnitBattleNumericValues) As Double
            Dim effHP As Double = CDbl(stats.hp) / (1 - 0.01 * CDbl(stats.armor))
            Dim d As Double = stats.damage + stats.heal
            If d = 0 Then d = 0.33 * effHP
            Dim effAction As Double = CDbl(stats.accuracy) * d
            Dim ini As Double = 1 + 0.02 * CDbl(stats.initiative - 50)
            Return effHP * effAction * ini
        End Function

        Public Shared Function Copy(ByRef v As UnitBattleNumericValues) As UnitBattleNumericValues
            Dim r As New UnitBattleNumericValues
            r.accuracy = v.accuracy
            r.armor = v.armor
            r.damage = v.damage
            r.heal = v.heal
            r.hp = v.hp
            r.initiative = v.initiative
            Return r
        End Function
    End Class
    Public Class UnitNumericValues
        Inherits UnitBattleNumericValues

        ''' <summary>Опыт за убийство юнита</summary>
        Public EXPkilled As Integer
        ''' <summary>Опыт для апа уровня</summary>
        Public EXPnext As Integer
        ''' <summary>Цена найма юнита</summary>
        Public unitCost As New Cost
    End Class

    Public Class Unit
        Inherits UnitNumericValues

        ''' <summary>Имя</summary>
        Public name As String
        ''' <summary>Базовый уровень</summary>
        Public level As Integer
        ''' <summary>Номер расы</summary>
        Public race As Integer
        ''' <summary>Лидерство от 0 до 6</summary>
        Public leadership As Integer
        ''' <summary>Область атаки.</summary>
        Public reach As Integer
        ''' <summary>GxxxUUxxxx</summary>
        Public unitID As String
        ''' <summary>True, если занимает одну клетку</summary>
        Public small As Boolean
        ''' <summary>True, если может находиться только на воде</summary>
        Public waterOnly As Boolean
        ''' <summary>Класс юнита</summary>
        Public unitBranch As Integer
        ''' <summary>Уровень, до которого статы растут согласно dynUpgr1</summary>
        Public dynUpgradeLevel As Integer
        ''' <summary>Рост статов до dynUpgradeLevel</summary>
        Public dynUpgrade1 As New DynUpgrade
        ''' <summary>Рост статов после dynUpgradeLevel</summary>
        Public dynUpgrade2 As New DynUpgrade
        ''' <summary>Можно ли использовать юнита. 0 - неизвестно, -1 - нет, 1 - да</summary>
        Friend useState As Integer
        ''' <summary>True - юнит находится в ветке развития играбельной расы</summary>
        Friend fromRaceBranch As Boolean

        ''' <summary>
        ''' Защита или иммунитет к источнику атаки.
        ''' Ключ - источник атаки, значение - тип сопротивления.
        ''' </summary>
        Public ASourceImmunity As New Dictionary(Of Integer, Integer)
        ''' <summary>
        ''' Защита или иммунитет к типу атаки.
        ''' Ключ - тип атаки, значение - тип сопротивления.
        ''' </summary>
        Public AClassImmunity As New Dictionary(Of Integer, Integer)

        Public Shared Shadows Function Copy(ByVal v As Unit) As Unit
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
                                  .dynUpgrade2 = DynUpgrade.Copy(v.dynUpgrade2), _
                                  .useState = v.useState, _
                                  .fromRaceBranch = v.fromRaceBranch, _
                                  .hp = v.hp, _
                                  .armor = v.armor, _
                                  .damage = v.damage, _
                                  .heal = v.heal, _
                                  .accuracy = v.accuracy, _
                                  .initiative = v.initiative, _
                                  .ASourceImmunity = CopyDictionaty(v.ASourceImmunity), _
                                  .AClassImmunity = CopyDictionaty(v.AClassImmunity)}
        End Function

        ''' <summary>Вернет прибавку к опыту за убийство юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetExpKilledOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.EXPkilled, dynUpgrade2.EXPkilled)
        End Function
        ''' <summary>Вернет прибавку к планке опыта юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetExpNextOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.EXPnext, dynUpgrade2.EXPnext)
        End Function
        ''' <summary>Вернет прибавку к здоровью юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetHPOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.hp, dynUpgrade2.hp)
        End Function
        ''' <summary>Вернет прибавку к броне юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetArmorOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.armor, dynUpgrade2.armor)
        End Function
        ''' <summary>Вернет прибавку к броне юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetDamageOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.damage, dynUpgrade2.damage)
        End Function
        ''' <summary>Вернет прибавку к броне юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetHealOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.heal, dynUpgrade2.heal)
        End Function
        ''' <summary>Вернет прибавку к броне юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetAccuracyOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.accuracy, dynUpgrade2.accuracy)
        End Function
        ''' <summary>Вернет прибавку к броне юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetInitiativeOverlevel(ByVal unitLevel As Integer) As Integer
            Return OverlevelValue(unitLevel, dynUpgrade1.initiative, dynUpgrade2.initiative)
        End Function
        ''' <summary>Вернет прибавку к стоимости найма юнита при получении им оверлевелов</summary>
        ''' <param name="unitLevel">Текущий уровень юнита</param>
        Public Function GetCostOverlevel(ByVal unitLevel As Integer) As Cost
            Return OverlevelValue(unitLevel, dynUpgrade1.unitCost, dynUpgrade2.unitCost)
        End Function

        Private Function OverlevelValue(ByVal unitLevel As Integer, _
                                        ByVal vDynUpgr1 As Integer, ByVal vDynUpgr2 As Integer) As Integer
            If level >= unitLevel Then Return 0
            Dim d1 As Integer = unitLevel - level
            If unitLevel < dynUpgradeLevel Then
                Return vDynUpgr1 * d1
            Else
                Dim d2 As Integer = dynUpgradeLevel - level
                Return vDynUpgr1 * d2 + vDynUpgr2 * (d1 - d2)
            End If
        End Function
        Private Function OverlevelValue(ByVal unitLevel As Integer, _
                                        ByVal vDynUpgr1 As Cost, ByVal vDynUpgr2 As Cost) As Cost
            If level >= unitLevel Then Return New Cost
            Dim d1 As Integer = unitLevel - level
            If unitLevel < dynUpgradeLevel Then
                Return vDynUpgr1 * d1
            Else
                Dim d2 As Integer = dynUpgradeLevel - level
                Return vDynUpgr1 * d2 + vDynUpgr2 * (d1 - d2)
            End If
        End Function

    End Class

    Public Class DynUpgrade
        Inherits UnitNumericValues

        Public Shared Shadows Function Copy(ByVal v As DynUpgrade) As DynUpgrade
            Return New DynUpgrade With {.EXPkilled = v.EXPkilled, _
                                        .EXPnext = v.EXPnext, _
                                        .unitCost = Cost.Copy(v.unitCost), _
                                        .hp = v.hp, _
                                        .armor = v.armor, _
                                        .damage = v.damage, _
                                        .heal = v.heal, _
                                        .accuracy = v.accuracy, _
                                        .initiative = v.initiative}
        End Function
    End Class

    Public Class Cost
        ''' <summary>Золото</summary>
        Public Gold As Integer
        ''' <summary>Мана Жизни</summary>
        Public Blue As Integer
        ''' <summary>Мана Преисподней</summary>
        Public Red As Integer
        ''' <summary>Мана Рун</summary>
        Public White As Integer
        ''' <summary>Мана Смерти</summary>
        Public Black As Integer
        ''' <summary>Мана Лесного Эликсира</summary>
        Public Green As Integer

        Public Shared Function Copy(ByVal v As Cost) As Cost
            If Not IsNothing(v) Then
                Return New Cost With {.Gold = v.Gold, _
                                      .Blue = v.Blue, _
                                      .Red = v.Red, _
                                      .White = v.White, _
                                      .Black = v.Black, _
                                      .Green = v.Green}
            Else
                Return New Cost
            End If
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
                Dim convertedValue As Integer = Math.Max(Math.Min(ValueConverter.StrToInt(v, costString, s1), 9999), 0)
                If s1 = "g" Then
                    res.Gold = convertedValue
                ElseIf s1 = "r" Then
                    res.Red = convertedValue
                ElseIf s1 = "y" Then
                    res.Blue = convertedValue
                ElseIf s1 = "e" Then
                    res.Black = convertedValue
                ElseIf s1 = "w" Then
                    res.White = convertedValue
                ElseIf s1 = "b" Then
                    res.Green = convertedValue
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
            If Not IsNothing(v) Then
                Dim ch() As String = New String() {"g", "r", "y", "e", "w", "b"}
                Dim val() As Integer = New Integer() {v.Gold, v.Red, v.Blue, v.Black, v.White, v.Green}
                Dim s As String = ""
                For i As Integer = 0 To UBound(ch) Step 1
                    s &= ch(i)
                    If val(i) < 0 Then val(i) = 0
                    If val(i) > 9999 Then
                        'Throw New Exception("Too great value of " & ch(i) & " : " & val(i))
                        val(i) = 9999
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
            Else
                Return Print(New Cost)
            End If
        End Function

        Public Shared Function ToArray(ByVal v As Cost) As Integer()
            If Not IsNothing(v) Then
                Return New Integer() {v.Gold, _
                                      v.Blue, _
                                      v.Red, _
                                      v.White, _
                                      v.Black, _
                                      v.Green}
            Else
                Return ToArray(New Cost)
            End If
        End Function
        Public Shared Function ToCost(ByVal v() As Integer) As Cost
            Return New Cost With {.Gold = v(0), _
                                  .Blue = v(1), _
                                  .Red = v(2), _
                                  .White = v(3), _
                                  .Black = v(4), _
                                  .Green = v(5)}
        End Function


        ''' <summary>Вернет суммарную стоимость в золоте и мане</summary>
        ''' <param name="v">цена: золото и мана</param>
        Public Shared Function Sum(ByVal v As Cost) As Integer
            If Not IsNothing(v) Then
                Return v.Gold + v.Black + v.Blue + v.Green + v.Red + v.White
            Else
                Return 0
            End If
        End Function

        Public Shared Operator *(ByVal v As Cost, ByVal n As Double) As Cost
            If Not IsNothing(v) Then
                Return New Cost With {.Black = CInt(v.Black * n), _
                                      .Blue = CInt(v.Blue * n), _
                                      .Gold = CInt(v.Gold * n), _
                                      .Green = CInt(v.Green * n), _
                                      .Red = CInt(v.Red * n), _
                                      .White = CInt(v.White * n)}
            Else
                Return New Cost
            End If
        End Operator
        Public Shared Operator *(ByVal n As Double, ByVal v As Cost) As Cost
            Return v * n
        End Operator
        Public Shared Operator *(ByVal v As Cost, ByVal n As Integer) As Cost
            If Not IsNothing(v) Then
                Return New Cost With {.Black = v.Black * n, _
                                      .Blue = v.Blue * n, _
                                      .Gold = v.Gold * n, _
                                      .Green = v.Green * n, _
                                      .Red = v.Red * n, _
                                      .White = v.White * n}
            Else
                Return New Cost
            End If
        End Operator
        Public Shared Operator *(ByVal n As Integer, ByVal v As Cost) As Cost
            Return v * n
        End Operator
        Public Shared Operator /(ByVal v As Cost, ByVal n As Double) As Cost
            If Not IsNothing(v) Then
                Return New Cost With {.Black = CInt(v.Black / n), _
                                      .Blue = CInt(v.Blue / n), _
                                      .Gold = CInt(v.Gold / n), _
                                      .Green = CInt(v.Green / n), _
                                      .Red = CInt(v.Red / n), _
                                      .White = CInt(v.White / n)}
            Else
                Return New Cost
            End If
        End Operator
        Public Shared Operator +(ByVal v1 As Cost, ByVal v2 As Cost) As Cost
            If Not IsNothing(v1) And Not IsNothing(v2) Then
                Return New Cost With {.Black = v1.Black + v2.Black, _
                                      .Blue = v1.Blue + v2.Blue, _
                                      .Gold = v1.Gold + v2.Gold, _
                                      .Green = v1.Green + v2.Green, _
                                      .Red = v1.Red + v2.Red, _
                                      .White = v1.White + v2.White}
            ElseIf Not IsNothing(v1) Then
                Return v1
            ElseIf Not IsNothing(v2) Then
                Return v2
            Else
                Return New Cost
            End If
        End Operator
        Public Shared Operator -(ByVal v1 As Cost, ByVal v2 As Cost) As Cost
            If Not IsNothing(v1) And Not IsNothing(v2) Then
                Return New Cost With {.Black = v1.Black - v2.Black, _
                                      .Blue = v1.Blue - v2.Blue, _
                                      .Gold = v1.Gold - v2.Gold, _
                                      .Green = v1.Green - v2.Green, _
                                      .Red = v1.Red - v2.Red, _
                                      .White = v1.White - v2.White}
            ElseIf Not IsNothing(v1) Then
                Return v1
            ElseIf Not IsNothing(v2) Then
                Return New Cost With {.Black = -v2.Black, _
                                      .Blue = -v2.Blue, _
                                      .Gold = -v2.Gold, _
                                      .Green = -v2.Green, _
                                      .Red = -v2.Red, _
                                      .White = -v2.White}
            Else
                Return New Cost
            End If
        End Operator

    End Class

    Public Class Item
        ''' <summary>Название</summary>
        Public name As String
        ''' <summary>GxxxIGxxxx</summary>
        Public itemID As String
        ''' <summary>Описание типов в ./Resources/Items.txt</summary>
        Public type As GenDefaultValues.ItemTypes
        ''' <summary>Цена покупки предмета. При продаже цена в пять раз меньше</summary>
        Public itemCost As New Cost
        ''' <summary>Сумма полей itemCost</summary>
        Friend itemCostSum As Double
        ''' <summary>Можно ли использовать предмет. 0 - неизвестно, -1 - нет, 1 - да</summary>
        Friend useState As Integer

        Public Shared Function Copy(ByVal v As Item) As Item
            Return New Item With {.name = v.name, _
                                  .itemID = v.itemID, _
                                  .type = v.type, _
                                  .itemCost = Cost.Copy(v.itemCost), _
                                  .itemCostSum = v.itemCostSum, _
                                  .useState = v.useState}
        End Function
    End Class

    Public Class Spell
        ''' <summary>ID заклинания</summary>
        Public spellID As String
        ''' <summary>Название заклинания</summary>
        Public name As String
        ''' <summary>Цена изучения для каждого лорда. Ключ - id лорда в верхнем регистре. Список лордов хранится в Common.LordsRace</summary>
        Public researchCost As New Dictionary(Of String, AllDataStructues.Cost)
        ''' <summary>Цена применения</summary>
        Public castCost As New AllDataStructues.Cost
        ''' <summary>Уровень заклинания</summary>
        Public level As Integer
        ''' <summary>Тип заклинания</summary>
        Public category As Integer
        ''' <summary>Площадь действия заклинания</summary>
        Public area As Integer

        Public Shared Function Copy(ByRef v As Spell) As Spell
            Dim r As New Dictionary(Of String, AllDataStructues.Cost)
            For Each k As String In v.researchCost.Keys
                r.Add(k, AllDataStructues.Cost.Copy(v.researchCost.Item(k)))
            Next k
            Return New Spell With {.area = v.area, _
                                    .castCost = AllDataStructues.Cost.Copy(v.castCost), _
                                    .category = v.category, _
                                    .level = v.level, _
                                    .name = v.name, _
                                    .spellID = v.spellID, _
                                    .researchCost = r}
        End Function
    End Class

    Public Class LootGenSettings
        ''' <summary>Cферы, талисманы и свитки</summary>
        Public ConsumableItems As ItemGenSettings
        ''' <summary>Надеваемые предметы и посохи</summary>
        Public NonconsumableItems As ItemGenSettings
        ''' <summary>Драгоценности</summary>
        Public JewelItems As ItemGenSettings
        ''' <summary>Добавит эти предметы в любом случае</summary>
        Public PreserveItems As List(Of String)
        ''' <summary>Установит стоимость добавляемого лута на 1.2*RandStack.minItemGoldCost, если LootCost меньше RandStack.minItemGoldCost</summary>
        Public addLootAnyway As Boolean
        ''' <summary>Множитель цены лута</summary>
        Friend lootCostMultiplier As Double
        ''' <summary>Строгий фильтр по типам</summary>
        Private typesFilter As StrictTypesFilter

        Public Class StrictTypesFilter
            ''' <summary>Количество, которое можно добавить по типу</summary>
            Public typesAmount() As Integer
            ''' <summary>Количество добавленных по типу</summary>
            Public typesAmountAdded() As Integer

            Private tAmountAllInAll As Integer
            Private tAmountAddedAllInAll As Integer

            ''' <param name="tAmount">Количество предметов каждого типа</param>
            Public Sub SetTypesAmountArray(Optional ByRef tAmount() As Integer = Nothing)
                If Not IsNothing(tAmount) Then
                    typesAmount = CType(tAmount.Clone, Integer())
                Else
                    ReDim typesAmount([Enum].GetValues(GetType(GenDefaultValues.ItemTypes)).Cast(Of Integer).Max)
                End If
                Call Initialize()
            End Sub

            ''' <summary>Перед использованием в создании предметов вызываем эту процедуру</summary>
            Public Sub Initialize()
                typesAmountAdded = Nothing
                tAmountAllInAll = 0
                If Not IsNothing(typesAmount) Then
                    ReDim typesAmountAdded(UBound(typesAmount))
                    For i As Integer = 0 To UBound(typesAmount) Step 1
                        tAmountAllInAll += 1
                    Next i
                End If
                tAmountAddedAllInAll = 0
            End Sub
            ''' <summary>Вернет True, если предмет подходит</summary>
            ''' <param name="item">Предмет</param>
            Public Function Filter(ByRef item As Item) As Boolean
                If IsNothing(typesAmount) Then Return True
                If tAmountAddedAllInAll >= tAmountAllInAll Then Return True
                If typesAmountAdded(item.type) >= typesAmount(item.type) Then Return False
                Return True
            End Function
            ''' <summary>Вызываем, когда предмет добвавлен</summary>
            ''' <param name="item">Предмет</param>
            Public Sub Added(ByRef item As Item)
                If Not IsNothing(typesAmountAdded) Then typesAmountAdded(item.type) += 1
                tAmountAddedAllInAll += 1
            End Sub
            ''' <summary>Вызываем, когда считаем количество предметов в луте</summary>
            ''' <param name="item">Предмет</param>
            Public Sub IncreaseTypeAmount(ByRef item As Item)
                If IsNothing(typesAmount) Then Exit Sub
                typesAmount(item.type) += 1
            End Sub


            Public Shared Function Copy(ByVal v As StrictTypesFilter) As StrictTypesFilter
                Return New StrictTypesFilter With {.tAmountAddedAllInAll = v.tAmountAddedAllInAll, _
                                                   .tAmountAllInAll = v.tAmountAllInAll, _
                                                   .typesAmount = CType(v.typesAmount.Clone, Integer()), _
                                                   .typesAmountAdded = CType(v.typesAmountAdded.Clone, Integer())}
            End Function
        End Class

        ''' <param name="useStrictFilter">Генератор предметов постарается создать предметы заданного типа</param>
        Public Sub New(ByVal useStrictFilter As Boolean)
            If useStrictFilter Then
                typesFilter = New StrictTypesFilter
            End If
        End Sub

        Public Shared Function Copy(ByVal v As LootGenSettings) As LootGenSettings
            Dim r As New LootGenSettings(False) _
                With {.ConsumableItems = AllDataStructues.ItemGenSettings.Copy(v.ConsumableItems), _
                      .NonconsumableItems = AllDataStructues.ItemGenSettings.Copy(v.NonconsumableItems), _
                      .JewelItems = AllDataStructues.ItemGenSettings.Copy(v.JewelItems), _
                      .PreserveItems = CopyList(v.PreserveItems), _
                      .lootCostMultiplier = v.lootCostMultiplier}
            If Not IsNothing(v.typesFilter) Then r.typesFilter = StrictTypesFilter.Copy(v.typesFilter)
            Return r
        End Function
        Public Shared Function Print(ByVal v As LootGenSettings) As String
            Dim p As String = PrintList(v.PreserveItems, "+")
            Dim f As String = ""
            If Not IsNothing(v.typesFilter) Then
                f = PrintList(v.typesFilter.typesAmount, " ")
            Else
                f = "no"
            End If

            Return "CItemsGen" & vbTab & AllDataStructues.ItemGenSettings.Print(v.ConsumableItems) & vbNewLine & _
                   "NItemsGen" & vbTab & AllDataStructues.ItemGenSettings.Print(v.NonconsumableItems) & vbNewLine & _
                   "JItemsGen" & vbTab & AllDataStructues.ItemGenSettings.Print(v.JewelItems) & vbNewLine & _
                   "PreservedItems" & vbTab & p & vbNewLine & _
                   "LootCostMultiplier" & vbTab & v.lootCostMultiplier & vbNewLine & _
                   "Strict types filter" & vbTab & f
        End Function
        ''' <summary>Return {ConsumableItems,NonconsumableItems,JewelItems}</summary>
        Public Shared Function ToArray(ByVal v As LootGenSettings) As ItemGenSettings()
            If Not IsNothing(v) Then
                Return New ItemGenSettings() {v.ConsumableItems, v.NonconsumableItems, v.JewelItems}
            Else
                Return ToArray(New AllDataStructues.LootGenSettings(False))
            End If
        End Function

#Region "Strict filter interface"
        ''' <param name="tAmount">Количество предметов каждого типа</param>
        Sub SetTypesAmountArray(Optional ByRef tAmount() As Integer = Nothing)
            If IsNothing(typesFilter) Then Exit Sub
            Call typesFilter.SetTypesAmountArray(tAmount)
        End Sub
        ''' <summary>Перед использованием в создании предметов вызываем эту процедуру</summary>
        Public Sub Initialize()
            If IsNothing(typesFilter) Then Exit Sub
            Call typesFilter.Initialize()
        End Sub
        ''' <summary>Вернет True, если предмет подходит</summary>
        ''' <param name="item">Предмет</param>
        Public Function Filter(ByRef item As Item) As Boolean
            If IsNothing(typesFilter) Then Return True
            Return typesFilter.Filter(item)
        End Function
        ''' <summary>Вызываем, когда предмет добвавлен</summary>
        ''' <param name="item">Предмет</param>
        Public Sub Added(ByRef item As Item)
            If IsNothing(typesFilter) Then Exit Sub
            Call typesFilter.Added(item)
        End Sub
        ''' <summary>Вызываем, когда считаем количество предметов в луте</summary>
        ''' <param name="item">Предмет</param>
        Public Sub IncreaseTypeAmount(ByRef item As Item)
            If IsNothing(typesFilter) Then Exit Sub
            Call typesFilter.IncreaseTypeAmount(item)
        End Sub
#End Region

    End Class

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

    Public Structure Restriction

        ''' <summary>Minimum value allowed</summary>
        Dim minimum As Double
        ''' <summary>Maximum value allowed</summary>
        Dim maximum As Double

        ''' <summary>Returns False, if v less than r.minimum or v greater than r.maximum </summary>
        Public Shared Function CheckValue(ByRef v As Double, ByRef r As Restriction) As Boolean
            If v < r.minimum OrElse v > r.maximum Then
                Return False
            Else
                Return True
            End If
        End Function

        Public Shared Function Copy(ByVal v As Restriction) As Restriction
            Return New Restriction With {.maximum = v.maximum, .minimum = v.minimum}
        End Function
    End Structure

    Public Class Modificator
        ''' <summary>
        ''' GxxxUMxxxx
        ''' </summary>
        Public id As String
        ''' <summary>
        ''' Тип модификатора.
        ''' Поле SOURCE в Gmodif
        ''' </summary>
        Public source As Type
        ''' <summary>
        ''' Список эффектов
        ''' </summary>
        Public effect As New List(Of ModifEffect)

        Public Enum Type
            ''' <summary>
            ''' L_UNIT
            ''' </summary>
            Defence = 0
            ''' <summary>
            ''' L_STACK
            ''' </summary>
            LeaderSkill = 1
            '  ''' <summary>
            '  ''' L_STACK_LEADER
            '  ''' </summary>
            '  L_STACK_LEADER = 2
            ''' <summary>
            ''' L_ATTACK
            ''' </summary>
            Offence = 3
        End Enum

        Public Class ModifEffect
            ''' <summary>
            ''' Тип модификатора
            ''' </summary>
            Public type As EffectType

            ''' <summary>
            ''' Поле PERCENT в GmodifL
            ''' </summary>
            Public percent As Integer
            ''' <summary>
            ''' Поле NUMBER в GmodifL
            ''' </summary>
            Public number As Integer
            ''' <summary>
            ''' Поле ABILITY в GmodifL
            ''' </summary>
            Public ability As Integer
            ''' <summary>
            ''' Поле IMMUNITY в GmodifL
            ''' </summary>
            Public immunASource As Integer
            ''' <summary>
            ''' Поле IMMUNECAT в GmodifL
            ''' </summary>
            Public immunASourceCat As Integer
            ''' <summary>
            ''' Поле IMMUNITYC в GmodifL
            ''' </summary>
            Public immunAClass As Integer
            ''' <summary>
            ''' Поле IMMUNECATC в GmodifL
            ''' </summary>
            Public immunAClassCat As Integer
            ''' <summary>
            ''' Поле Move в GmodifL
            ''' </summary>
            Public move As Integer

            Public Enum EffectType
                ScoutingRange = 1
                Leadership = 2
                Accuracy = 3
                Damage = 4
                Armor = 5
                HitPoints = 6
                MovePoints = 7
                Initiative = 9
                MoveAbility = 10
                LeaderAbility = 11
                ImmunitySource = 12
                Regeneration = 13
                ImmunityClass = 14
                Vampirism = 15
                FastRetreat = 16
                LowerPurchaseCost = 17
            End Enum
        End Class

        Public Shared Function TotalLeadershipChange(ByRef modificators As List(Of String), _
                                                     ByRef randStack As RandStack) As Integer
            Dim n As Integer = 0
            If Not IsNothing(modificators) Then
                For Each m As String In modificators
                    Dim modif As AllDataStructues.Modificator = randStack.FindModificatorStats(m)
                    If Not IsNothing(modif) Then n += modif.GetLeadershipChange
                Next m
            End If
            Return n
        End Function
        Public Function GetLeadershipChange() As Integer
            For Each e As ModifEffect In effect
                If e.type = ModifEffect.EffectType.Leadership Then Return e.number
            Next e
            Return 0
        End Function

        Public Shared Function UnitPowerChange(ByRef unit As Stack.UnitInfo, ByRef R As RandStack) As Double
            If unit.modificators.Count = 0 Then Return 1
            Dim b As UnitBattleNumericValues = UnitBattleNumericValues.BaseBattleNumericStats(unit)
            Dim p As Double = UnitBattleNumericValues.UnitPower(b)
            Dim m As New List(Of Modificator)
            Call AddModificators(m, unit.modificators, R)
            Return UnitPowerChange(unit.unit, b, p, m)
        End Function
        Public Shared Function UnitPowerChange(ByRef unit As Unit, _
                                               ByRef base As UnitBattleNumericValues, _
                                               ByRef baseUnitPower As Double, _
                                               ByRef baseModificators As List(Of Modificator), _
                                               ByRef basePowerChange As Double, _
                                               ByRef modificator As Modificator) As Double
            Dim m As New List(Of Modificator)
            m.Add(modificator)
            Return UnitPowerChange(unit, base, baseUnitPower, baseModificators, basePowerChange, m)
        End Function
        Public Shared Function UnitPowerChange(ByRef unit As Unit, _
                                               ByRef base As UnitBattleNumericValues, _
                                               ByRef baseUnitPower As Double, _
                                               ByRef baseModificators As List(Of Modificator), _
                                               ByRef basePowerChange As Double, _
                                               ByRef modificators As List(Of Modificator)) As Double
            If modificators.Count = 0 Then Return 1
            Dim m As New List(Of Modificator)
            Call AddModificators(m, baseModificators)
            Call AddModificators(m, modificators)
            Dim n2 As Double = UnitPowerChange(unit, base, baseUnitPower, m)
            Return n2 / basePowerChange
        End Function
        Public Shared Function UnitPowerChange(ByRef unit As Unit, _
                                               ByRef base As UnitBattleNumericValues, _
                                               ByRef baseUnitPower As Double, _
                                               ByRef modificators As List(Of Modificator)) As Double
            Dim r As Double = 1
            If modificators.Count > 0 Then
                Dim modif As UnitBattleNumericValues = UnitNumericValues.Copy(base)
                For Each m As Modificator In modificators
                    Call ModifyNumericStats(m.effect, modif)
                Next m
                Dim n As Double = UnitBattleNumericValues.UnitPower(modif) / baseUnitPower
                Dim w As Double = WardsEffect(unit, TotalSourceWards(modificators), TotalClassWards(modificators))
                r = n * w
            End If
            Return r
        End Function
        Public Shared Sub AddModificators(ByRef dest As List(Of Modificator), ByRef content As List(Of String), ByRef R As RandStack)
            For Each id As String In content
                dest.Add(R.FindModificatorStats(id))
            Next id
        End Sub
        Public Shared Sub AddModificators(ByRef dest As List(Of Modificator), ByRef content As List(Of Modificator))
            For Each modif As Modificator In content
                dest.Add(modif)
            Next modif
        End Sub

        Private Shared Sub ModifyNumericStats(ByRef effect As List(Of ModifEffect), _
                                              ByRef modif As UnitBattleNumericValues)

            For Each e As ModifEffect In effect
                If e.type = ModifEffect.EffectType.Accuracy Then
                    Call MultiplicativeEffect(modif.accuracy, e.percent, 100)
                ElseIf e.type = ModifEffect.EffectType.Armor Then
                    Call AdditiveEffect(modif.armor, e.number)
                ElseIf e.type = ModifEffect.EffectType.Damage Then
                    Call MultiplicativeEffect(modif.damage, e.percent)
                ElseIf e.type = ModifEffect.EffectType.HitPoints Then
                    If e.number = 1 Then
                        Call MultiplicativeEffect(modif.hp, e.percent)
                    ElseIf e.number = 2 Then
                        Call AdditiveEffect(modif.hp, e.percent)
                    Else
                        Throw New Exception("Unexpected NUMBER field in type " & e.type)
                    End If
                ElseIf e.type = ModifEffect.EffectType.Initiative Then
                    Call MultiplicativeEffect(modif.initiative, e.percent, 150)
                ElseIf e.type = ModifEffect.EffectType.Vampirism Then
                    Call MultiplicativeEffect(modif.damage, CInt(0.5 * e.percent))
                End If
            Next e
        End Sub
        Private Shared Function TotalSourceWards(ByRef modificators As List(Of Modificator)) As Dictionary(Of Integer, Integer)
            Dim result As New Dictionary(Of Integer, Integer)
            For Each m As Modificator In modificators
                Dim r As Dictionary(Of Integer, Integer) = m.GetSourceWards
                If r.Count > 0 Then
                    For Each k As Integer In r.Keys
                        If Not result.ContainsKey(k) Then
                            result.Add(k, r.Item(k))
                        ElseIf r.Item(k) > result.Item(k) Then
                            result.Remove(k)
                            result.Add(k, r.Item(k))
                        End If
                    Next k
                End If
            Next m
            Return result
        End Function
        Private Function GetSourceWards() As Dictionary(Of Integer, Integer)
            Dim result As New Dictionary(Of Integer, Integer)
            For Each e As ModifEffect In effect
                If e.type = ModifEffect.EffectType.ImmunitySource Then
                    result.Add(e.immunASource, e.immunASourceCat)
                End If
            Next e
            Return result
        End Function
        Private Shared Function TotalClassWards(ByRef modificators As List(Of Modificator)) As Dictionary(Of Integer, Integer)
            Dim result As New Dictionary(Of Integer, Integer)
            For Each m As Modificator In modificators
                Dim r As Dictionary(Of Integer, Integer) = m.GetClassWards
                If r.Count > 0 Then
                    For Each k As Integer In r.Keys
                        If Not result.ContainsKey(k) Then
                            result.Add(k, r.Item(k))
                        ElseIf r.Item(k) > result.Item(k) Then
                            result.Remove(k)
                            result.Add(k, r.Item(k))
                        End If
                    Next k
                End If
            Next m
            Return result
        End Function
        Private Function GetClassWards() As Dictionary(Of Integer, Integer)
            Dim result As New Dictionary(Of Integer, Integer)
            For Each e As ModifEffect In effect
                If e.type = ModifEffect.EffectType.ImmunityClass Then
                    result.Add(e.immunAClass, e.immunAClassCat)
                End If
            Next e
            Return result
        End Function
        Private Shared Function WardsEffect(ByRef unit As Unit, _
                                            ByRef sourceWards As Dictionary(Of Integer, Integer), _
                                            ByRef classWards As Dictionary(Of Integer, Integer)) As Double
            Dim multiplier As Double = 1
            For Each k As Integer In sourceWards.Keys
                Dim type As Integer = k
                Dim cat As Integer = sourceWards.Item(k)
                Dim resBonus As Double = 1.09
                Dim immBonus As Double = 1.15
                If unit.ASourceImmunity.ContainsKey(type) Then
                    If cat > unit.ASourceImmunity.Item(type) Then
                        multiplier *= immBonus / resBonus
                    End If
                Else
                    If cat = 2 Then
                        multiplier *= resBonus
                    ElseIf cat = 3 Then
                        multiplier *= immBonus
                    Else
                        Throw New Exception("Unexpected IMMUNECAT field in type " & type)
                    End If
                End If
            Next k
            For Each k As Integer In classWards.Keys
                Dim type As Integer = k
                Dim cat As Integer = classWards.Item(k)
                Dim resBonus As Double = 1.04
                Dim immBonus As Double = 1.06
                If unit.AClassImmunity.ContainsKey(type) Then
                    If cat > unit.AClassImmunity.Item(type) Then
                        multiplier *= immBonus / resBonus
                    End If
                Else
                    If cat = 2 Then
                        multiplier *= resBonus
                    ElseIf cat = 3 Then
                        multiplier *= immBonus
                    Else
                        Throw New Exception("Unexpected IMMUNECATC field in type " & type)
                    End If
                End If
            Next k
            Return multiplier
        End Function

        Private Shared Sub MultiplicativeEffect(ByRef currentValue As Integer, ByRef modifValue As Integer, _
                                                Optional ByVal Cap As Integer = -1)
            Dim v As Integer = CInt(Math.Floor(CDbl(currentValue) * (1 + 0.01 * CDbl(modifValue))))
            If Cap > -1 Then v = Math.Min(v, Cap)
            currentValue = v
        End Sub
        Private Shared Sub AdditiveEffect(ByRef currentValue As Integer, ByRef modifValue As Integer, _
                                          Optional ByVal Cap As Integer = -1)
            Dim v As Integer = currentValue + modifValue
            If Cap > -1 Then v = Math.Min(v, Cap)
            currentValue = v
        End Sub

    End Class

    Public MustInherit Class CommonCreationSettings
        '''<summary>Точка на карте, в которую добавляются предметы или отряд</summary>
        Public pos As Point
    End Class
    Public Class CommonStackCreationSettings
        Inherits CommonCreationSettings

        '''<summary>Желаемые параметры стэка</summary>
        Public StackStats As AllDataStructues.DesiredStats
        '''<summary>Изменение лидерства за счет модификаторов</summary>
        Public deltaLeadership As Integer
        '''<summary>True, если на клетку нельзя ставить водных лидеров. Водной считается клетка с водой, окруженная со всех сторон клетками с водой</summary>
        Public groundTile As Boolean
        '''<summary>True, если стэк находится внутри руин или города</summary>
        Public noLeader As Boolean
        '''<summary>True, если создаем шаблон отряда</summary>
        Public isTemplate As Boolean
        '''<summary>Приказ отряда</summary>
        Public order As String

        Public Shared Function Copy(ByVal v As CommonStackCreationSettings) As CommonStackCreationSettings
            Return New CommonStackCreationSettings With {.StackStats = DesiredStats.Copy(v.StackStats), _
                                                         .deltaLeadership = v.deltaLeadership, _
                                                         .groundTile = v.groundTile, _
                                                         .noLeader = v.noLeader, _
                                                         .pos = New Point(v.pos.X, v.pos.Y), _
                                                         .isTemplate = v.isTemplate, _
                                                         .order = v.order}
        End Function
    End Class
    Public Class CommonLootCreationSettings
        Inherits CommonCreationSettings

        '''<summary>Максимальная стоимость набора в золоте. Драгоценности считаются дешевле в два раза</summary>
        Public GoldCost As Integer
        '''<summary>Настройки генерации предметов</summary>
        Public IGen As AllDataStructues.LootGenSettings
        '''<summary>Ключ - тип предмета, Значение - ограничение стоимости. Игнорируется, если массив неинициализирован</summary>
        Public TypeCostRestriction As Dictionary(Of Integer, AllDataStructues.Restriction)

        Public Shared Function Copy(ByVal v As CommonLootCreationSettings) As CommonLootCreationSettings
            Dim t As Dictionary(Of Integer, AllDataStructues.Restriction) = Nothing
            If Not IsNothing(v.TypeCostRestriction) Then
                t = New Dictionary(Of Integer, AllDataStructues.Restriction)
                For Each k As Integer In v.TypeCostRestriction.Keys
                    t.Add(k, Restriction.Copy(v.TypeCostRestriction.Item(k)))
                Next k
            End If
            Return New CommonLootCreationSettings With {.GoldCost = v.GoldCost, _
                                                        .IGen = LootGenSettings.Copy(v.IGen), _
                                                        .pos = New Point(v.pos.X, v.pos.Y), _
                                                        .TypeCostRestriction = t}
        End Function
    End Class

#Region "Вспомогательные функции"
    Private Shared Function CopyList(ByRef v As List(Of String)) As List(Of String)
        Dim result As List(Of String) = Nothing
        If Not IsNothing(v) Then
            result = New List(Of String)
            For Each Item As String In v
                result.Add(Item.ToUpper)
            Next Item
        End If
        Return result
    End Function
    Private Shared Function CopyList(ByRef v As List(Of Stack.UnitInfo)) As List(Of Stack.UnitInfo)
        Dim result As List(Of Stack.UnitInfo) = Nothing
        If Not IsNothing(v) Then
            result = New List(Of Stack.UnitInfo)
            For Each Item As Stack.UnitInfo In v
                result.Add(Stack.UnitInfo.Copy(Item))
            Next Item
        End If
        Return result
    End Function
    Private Shared Function CopyList(ByRef v As List(Of Integer)) As List(Of Integer)
        Dim result As List(Of Integer) = Nothing
        If Not IsNothing(v) Then
            result = New List(Of Integer)
            For Each Item As Integer In v
                result.Add(Item)
            Next Item
        End If
        Return result
    End Function

    Private Shared Function CopyDictionaty(ByRef v As Dictionary(Of Integer, Integer)) As Dictionary(Of Integer, Integer)
        Dim result As Dictionary(Of Integer, Integer) = Nothing
        If Not IsNothing(v) Then
            result = New Dictionary(Of Integer, Integer)
            Dim keys As List(Of Integer) = v.Keys.ToList
            For Each k As Integer In keys
                result.Add(k, v.Item(k))
            Next k
        End If
        Return result
    End Function

    Private Shared Function PrintList(ByRef v As List(Of Integer), ByRef delimiter As String, _
                                      ByRef valueToStringValue As Dictionary(Of Integer, String)) As String
        Dim f As String = ""
        For Each Item As Integer In v
            If Not f = "" Then f &= delimiter
            If Not IsNothing(valueToStringValue) Then
                f &= valueToStringValue.Item(Item)
            Else
                f &= Item
            End If
        Next Item
        Return f
    End Function
    Private Shared Function PrintList(ByRef v As List(Of String), ByRef delimiter As String) As String
        Dim f As String = ""
        If Not IsNothing(v) AndAlso v.Count > 0 Then
            For Each item As String In v
                If Not f = "" Then f &= delimiter
                f &= item
            Next item
        Else
            f = "no"
        End If
        Return f
    End Function
    Private Shared Function PrintList(ByRef v As List(Of Stack.UnitInfo), ByRef delimiter As String) As String
        Dim f As String = ""
        If Not IsNothing(v) AndAlso v.Count > 0 Then
            For Each item As Stack.UnitInfo In v
                If Not f = "" Then f &= delimiter
                f &= Stack.UnitInfo.Print(item)
            Next item
        Else
            f = "no"
        End If
        Return f
    End Function
    Private Shared Function PrintList(ByRef v() As Integer, ByRef delimiter As String) As String
        Dim f As String = ""
        If Not IsNothing(v) AndAlso v.Count > 0 Then
            For i As Integer = 0 To UBound(v) Step 1
                If Not f = "" Then f &= delimiter
                f &= v(i)
            Next i
        Else
            f = "no"
        End If
        Return f
    End Function
#End Region
End Class

Public Class ValueConverter

    Friend Shared Function StrToDbl(ByRef s As String) As Double
        Return CDbl(ValueConverter.StrToDblStr(s))
        'Return Convert.ToDouble(s, Globalization.NumberFormatInfo.InvariantInfo)
    End Function
    Friend Shared Function StrToDbl(ByRef v As String, ByRef fullLine As String, ByRef fieldName As String) As Double
        Try
            Return ValueConverter.StrToDbl(v)
        Catch ex As Exception
            Dim msg As String = ex.Message & vbNewLine & fullLine
            If Not fieldName = "" Then msg &= vbNewLine & "Field: " & fieldName
            Throw New Exception(msg)
            Return 1
        End Try
    End Function

    Friend Shared Function StrToDblStr(ByRef s As String) As String
        Return s.Replace(",", ".").Replace(".", System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator)
    End Function

    Friend Shared Function StrToInt(ByRef v As String, ByRef fullLine As String, ByRef fieldName As String) As Integer
        Try
            Return CInt(v)
        Catch ex As Exception
            Dim msg As String = ex.Message & vbNewLine & fullLine
            If Not fieldName = "" Then msg &= vbNewLine & "Field: " & fieldName
            Throw New Exception(msg)
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

    Friend Shared Function IntToByteArray(ByRef v As Integer) As String
        Dim res As String = ""
        Dim bytes() As Byte = BitConverter.GetBytes(v)
        For j As Integer = 0 To 3 Step 1
            res &= Chr(bytes(j))
        Next j
        Return res
    End Function

    Friend Shared Function BitJoin(ByRef v1 As Integer, ByRef v2 As Integer) As Long
        Return CLng(v1) Or CLng(v2) << 32
    End Function
    Friend Shared Function BitJoin(ByRef v1 As Short, ByRef v2 As Short) As Integer
        Return CInt(v1) Or CInt(v2) << 16
    End Function

    Friend Shared Function ToChrString(ByRef v As Long) As String
        Dim bytes() As Byte = BitConverter.GetBytes(v)
        Return Chr(bytes(0)) & _
               Chr(bytes(1)) & _
               Chr(bytes(2)) & _
               Chr(bytes(3)) & _
               Chr(bytes(4)) & _
               Chr(bytes(5)) & _
               Chr(bytes(6)) & _
               Chr(bytes(7))
    End Function
    Friend Shared Function ToChrString(ByRef v As Integer) As String
        Dim bytes() As Byte = BitConverter.GetBytes(v)
        Return Chr(bytes(0)) & _
               Chr(bytes(1)) & _
               Chr(bytes(2)) & _
               Chr(bytes(3))
    End Function

    Friend Shared Function BoolArrayToStr(ByRef v() As Boolean) As String
        Return BoolArrayToStr(v, UBound(v))
    End Function
    Friend Shared Function BoolArrayToStr(ByRef v() As Boolean, ByRef convertTo As Integer) As String
        Dim vout As Byte = 0
        Dim bitPos As Integer = 0
        Dim strOut As String = ""
        For i As Integer = 0 To convertTo Step 1
            If v(i) Then vout = vout Or CByte(1 << bitPos)
            bitPos += 1
            If bitPos = 8 Then
                strOut &= Chr(vout)
                bitPos = 0
                vout = 0
            End If
        Next i
        If bitPos > 0 Then strOut &= Chr(vout)
        Return strOut
    End Function

    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    Public Shared Function TxtSplit(ByRef TXT() As String) As String()
        Return TxtSplit(String.Join(vbNewLine, TXT))
    End Function
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    Public Shared Function TxtSplit(ByRef TXT As String) As String()
        Dim splited() As String = TXT.Replace(Chr(10), Chr(13)).Replace(vbTab, " ").Split(Chr(13))
        Dim parseString(UBound(splited)) As Boolean
        Dim nStrings As Integer = -1
        For i As Integer = 0 To UBound(splited) Step 1
            If splited(i).Length > 0 AndAlso Not splited(i).Substring(0, 1) = "#" Then
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
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    ''' <param name="transferChar">Если строка заканчивается этой подстрокой, то подстрока удаляется, а текущая строка объединяется со следующей</param>
    Public Shared Function TxtSplit(ByRef TXT() As String, ByRef transferChar As String) As String()
        Return TxtSplit(String.Join(vbNewLine, TXT), transferChar)
    End Function
    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив строки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    ''' <param name="transferChar">Если строка заканчивается этой подстрокой, то подстрока удаляется, а текущая строка объединяется со следующей</param>
    Public Shared Function TxtSplit(ByRef TXT As String, ByRef transferChar As String) As String()
        Dim t() As String = ValueConverter.TxtSplit(TXT)
        Dim result(UBound(t)) As String
        Dim n As Integer = 0
        Dim m As Integer
        Dim newline As Boolean = True
        For i As Integer = 0 To UBound(t)
            If newline Then
                result(n) = t(i)
                newline = False
                m = n
            Else
                If result(n).EndsWith(transferChar) Then
                    result(n) = result(n).Substring(0, result(n).Length - transferChar.Length) & t(i)
                Else
                    i -= 1
                    n += 1
                    newline = True
                End If
            End If
        Next i
        If m < UBound(result) Then ReDim Preserve result(m)
        Return result
    End Function

End Class

Public Class GenDefaultValues

    Public Const DefaultMod As String = "MNS"
    Public Const myVersion As String = "06.10.2021.20.08"

    Public Shared Function PrintVersion() As String
        Return "Semga's generator DLL version: " & myVersion
    End Function

    ''' <summary>Название выбранного мода</summary>
    ''' <remarks></remarks>
    Public ReadOnly selectedMod As String
    Private myLog As Log

    Protected Friend linked_Races As New Dictionary(Of String, Integer)
    Friend RaceNumberToRaceChar As New Dictionary(Of Integer, String)

    ''' <param name="log">Лог для записи отчета, можно nothing</param>
    ''' <param name="modName">Название мода, на котором происходит генерация.
    ''' Ваианты можно получить из GenDefaultValues.GetSupportedMods</param>
    ''' <remarks></remarks>
    Public Sub New(ByRef log As Log, ByVal modName As String)

        myLog = log
        selectedMod = modName
        If Not GetSupportedMods().Contains(selectedMod) Then Throw New Exception("Unexpected mod name: " & selectedMod)

        Dim itemTypeID As New Dictionary(Of String, Integer)
        Dim itemTypeName As New Dictionary(Of Integer, String)
        Call ParseItemTypes(itemTypeID, itemTypeName)
        Call ParseRaces(linked_Races, RaceNumberToRaceChar)

        '#####################
        Dim allRaces() As String = ValueConverter.TxtSplit(Races)
        Dim allLords() As String = ValueConverter.TxtSplit(Lords)

        Dim lordsRaceID As New List(Of Integer)
        For Each line As String In allLords
            Dim s As Integer = linked_Races.Item(line.Split(CChar(" "))(1))
            If Not lordsRaceID.Contains(s) Then lordsRaceID.Add(s)
        Next line
        ReDim playableRaces(-1), neutralRaces(-1)
        For Each line As String In allRaces
            Dim s As String = line.Split(CChar(" "))(0)
            If lordsRaceID.Contains(linked_Races.Item(s.ToUpper)) Then
                ReDim Preserve playableRaces(playableRaces.Length)
                playableRaces(UBound(playableRaces)) = s.ToUpper
            Else
                ReDim Preserve neutralRaces(neutralRaces.Length)
                neutralRaces(UBound(neutralRaces)) = s.ToUpper
            End If
        Next line

        Dim capitalRace() As String = ValueConverter.TxtSplit(Capitals)
        generatorRaceToGameRace = New Dictionary(Of String, String)
        generatorRaceToCapitalID = New Dictionary(Of String, String)
        capitalToGeneratorRace = New Dictionary(Of String, String)
        gameRaceToGeneratorRace = New Dictionary(Of String, String)
        For Each line As String In capitalRace
            Dim s() As String = line.Split(CChar(" "))
            Dim cap As String = s(0).ToUpper
            Dim genR As String = RaceNumberToRaceChar.Item(linked_Races.Item(s(1).ToUpper))
            Dim gameR As String = s(2).ToUpper
            generatorRaceToGameRace.Add(genR, gameR)
            generatorRaceToCapitalID.Add(genR, cap)
            capitalToGeneratorRace.Add(cap, genR)
            gameRaceToGeneratorRace.Add(gameR, genR)
        Next line
        '#####################

        Dim RConstants As Dictionary(Of String, String) = PropertiesArrayToDictionary(ValueConverter.TxtSplit(Constants(), "/"))
        Dim DConstants As Dictionary(Of String, String) = PropertiesArrayToDictionary(ValueConverter.TxtSplit(Common.RecursiveReadFile( _
                                                            My.Resources.Constants.Replace(Chr(13), Chr(10)).Split(CChar(Chr(10)))), "/"))

        Dim HasSynonyms, sendLinkedRaces As New List(Of String)
        HasSynonyms.AddRange({"StackRaceChance".ToUpper})
        sendLinkedRaces.AddRange({"StackRaceChance".ToUpper})


        Dim fields() As String = ClassFieldsHandler.GetFieldsNamesList(Me, {"myLog", "linked_Races", "RaceNumberToRaceChar", "playableRaces", "neutralRaces", _
                                                                            "generatorRaceToGameRace", "generatorRaceToCapitalID", "capitalToGeneratorRace", _
                                                                            "gameRaceToGeneratorRace", "selectedMod", "maxItemTypeID"})

        For Each f As String In fields
            If sendLinkedRaces.Contains(f.ToUpper) Then
                Call SetProperty(ClassFieldsHandler.GetField(Me, f), RConstants, DConstants, linked_Races, HasSynonyms.Contains(f.ToUpper))
            Else
                Call SetProperty(ClassFieldsHandler.GetField(Me, f), RConstants, DConstants, itemTypeID, HasSynonyms.Contains(f.ToUpper))
            End If
        Next f

        Dim commonRacesBlock As String = ""
        Call SetProperty(commonRacesBlock, "commonRacesBlock", RConstants, DConstants)

        For i As Integer = 0 To UBound(LocRacesBlocks) Step 1
            If Not LocRacesBlocks(i).EndsWith(":") Then LocRacesBlocks(i) &= ","
            LocRacesBlocks(i) = (LocRacesBlocks(i) & commonRacesBlock).Replace("*", ",")
        Next i

        If Not IsNothing(log) AndAlso log.IsEnabled Then

            Dim spaces As String = "       "
            Dim splitValue As New List(Of String)
            Dim delimiter As Char
            splitValue.AddRange({"Global_ItemType_ChanceMultiplier".ToUpper})

            Dim StackRaceChanceStr As String = ""
            For Each i As Integer In RaceNumberToRaceChar.Keys
                StackRaceChanceStr &= vbNewLine & spaces & _
                  RaceNumberToRaceChar.Item(i) & " (" & i & ") = " & StackRaceChance(i)
            Next i

            For Each f As String In fields
                If splitValue.Contains(f.ToUpper) Then
                    delimiter = CChar(";")
                Else
                    delimiter = CChar("")
                End If
                log.Add(PrintItemsSettings(ClassFieldsHandler.GetField(Me, f), itemTypeName, spaces, delimiter))
            Next f
        End If

    End Sub
    ''' <summary>Вернет список поддерживаемых модов</summary>
    Public Shared Function GetSupportedMods() As String()
        Dim f() As String = IO.Directory.GetDirectories(".\Resources", My.Resources.modSettings_keyword & "*")
        For i As Integer = 0 To UBound(f) Step 1
            f(i) = IO.Path.GetFileName(f(i)).Substring(My.Resources.modSettings_keyword.Length)
        Next i
        Return f
    End Function

#Region "Resources parsing"
    Private Function PrintItemsSettings(ByRef name As String, ByRef settings() As String, ByRef spaces As String) As String
        Dim result As String = name & " ="
        For Each i As String In settings
            result &= vbNewLine & spaces & i
        Next i
        Return result
    End Function
    Private Function PrintItemsSettings(ByRef name As String, ByRef settings() As Integer, ByRef itemTypeName As Dictionary(Of Integer, String), ByRef spaces As String) As String
        Dim result As String = name & " ="
        Dim keys(itemTypeName.Count - 1) As Integer
        itemTypeName.Keys.CopyTo(keys, 0)
        For Each i As Integer In keys
            result &= vbNewLine & spaces & PrintItemsSettings(itemTypeName.Item(i), settings(i))
        Next i
        Return result
    End Function
    Private Function PrintItemsSettings(ByRef name As String, ByRef settings() As Double, ByRef itemTypeName As Dictionary(Of Integer, String), ByRef spaces As String) As String
        Dim result As String = name & " ="
        Dim keys(itemTypeName.Count - 1) As Integer
        itemTypeName.Keys.CopyTo(keys, 0)
        For Each i As Integer In keys
            result &= vbNewLine & spaces & PrintItemsSettings(itemTypeName.Item(i), settings(i))
        Next i
        Return result
    End Function
    Private Function PrintItemsSettings(ByRef name As String, ByRef value As String) As String
        Return name & " = " & value
    End Function
    Private Function PrintItemsSettings(ByRef name As String, ByRef value As Integer) As String
        Return PrintItemsSettings(name, value.ToString)
    End Function
    Private Function PrintItemsSettings(ByRef name As String, ByRef value As Double) As String
        Return PrintItemsSettings(name, value.ToString)
    End Function
    Private Function PrintItemsSettings(ByRef valueField As ClassFieldsHandler.GetFieldResult, ByRef itemTypeName As Dictionary(Of Integer, String), ByRef spaces As String, Optional ByRef splitBy As Char = CChar("")) As String
        If valueField.searchResultField.FieldType.FullName = GetType(String).FullName Then
            Dim val As String = CType(valueField.searchResultField.GetValue(valueField.LastParent), String)
            If splitBy = CChar("") Then
                Return PrintItemsSettings(valueField.searchResultField.Name, val)
            Else
                Return PrintItemsSettings(valueField.searchResultField.Name, val.Split(splitBy), spaces)
            End If
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(String()).FullName Then
            Return PrintItemsSettings(valueField.searchResultField.Name, CType(valueField.searchResultField.GetValue(valueField.LastParent), String()), spaces)
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Double).FullName Then
            Return PrintItemsSettings(valueField.searchResultField.Name, CType(valueField.searchResultField.GetValue(valueField.LastParent), Double))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Double()).FullName Then
            Return PrintItemsSettings(valueField.searchResultField.Name, CType(valueField.searchResultField.GetValue(valueField.LastParent), Double()), itemTypeName, spaces)
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Integer()).FullName Then
            Return PrintItemsSettings(valueField.searchResultField.Name, CType(valueField.searchResultField.GetValue(valueField.LastParent), Integer()), itemTypeName, spaces)
        Else
            Throw New Exception("Unexpected variable type")
        End If
    End Function
    Private Function ChangeToSynonymicNames(ByRef input As Dictionary(Of String, String), _
                                            ByRef synonyms As Dictionary(Of String, Integer)) As Dictionary(Of String, String)
        Dim res As New Dictionary(Of String, String)
        For Each key As String In input.Keys
            Dim s As String = synonyms.Item(key).ToString.ToUpper
            If res.ContainsKey(s) Then res.Remove(s)
            res.Add(s, input.Item(key))
        Next key
        Return res
    End Function
    Private Function PropertiesArrayToDictionary(ByRef input() As String) As Dictionary(Of String, String)
        Dim res As New Dictionary(Of String, String)
        Dim k As String
        For Each line As String In input
            Dim s() As String = line.Split(CChar(" "))
            If s.Length = 2 Then
                k = s(0).ToUpper
                If res.ContainsKey(k) Then res.Remove(k)
                res.Add(k, s(1))
            End If
        Next line
        Return res
    End Function
    Private Function GetProperty(ByRef name As String, ByRef readValues As Dictionary(Of String, String), _
                                 ByRef defaultValues As Dictionary(Of String, String)) As String
        name = name.ToUpper
        If readValues.ContainsKey(name) Then
            Return readValues.Item(name)
        ElseIf defaultValues.ContainsKey(name) Then
            Return defaultValues.Item(name)
        Else
            Throw New Exception("Unexpected property name " & name)
        End If
    End Function
    Private Function SetProperty(ByRef output As Double, ByRef name As String, ByRef readValues As Dictionary(Of String, String), ByRef defaultValues As Dictionary(Of String, String)) As Double
        output = ValueConverter.StrToDbl(GetProperty(name, readValues, defaultValues))
        Return output
    End Function
    Private Function SetProperty(ByRef output() As Double, ByRef name As String, ByRef readValues As Dictionary(Of String, String), _
                                 ByRef defaultValues As Dictionary(Of String, String), ByRef nameToID As Dictionary(Of String, Integer), _
                                 ByRef HasSynonyms As Boolean) As Double()
        Dim r, d As Dictionary(Of String, String)
        r = Nothing : d = Nothing
        Dim nonsynomical As Dictionary(Of String, Integer) = Nothing
        Call SetPropertyMakeIn(nonsynomical, r, d, name, readValues, defaultValues, nameToID, HasSynonyms)

        ReDim output(nonsynomical.Values.Max)
        For Each key As String In nonsynomical.Keys
            Dim v As Double = ValueConverter.StrToDbl(GetProperty(key, r, d))
            output(nonsynomical.Item(key)) = v
        Next key

        If HasSynonyms Then
            For i As Integer = 0 To UBound(output) Step 1
                If output(i) = 0 AndAlso nameToID.ContainsKey(i.ToString) Then
                    Dim source As Integer = CInt(nameToID.Item(i.ToString))
                    output(i) = output(source)
                End If
            Next i
        End If
        Return output
    End Function
    Private Function SetProperty(ByRef output() As Integer, ByRef name As String, ByRef readValues As Dictionary(Of String, String), _
                                 ByRef defaultValues As Dictionary(Of String, String), ByRef nameToID As Dictionary(Of String, Integer), _
                                 ByRef HasSynonyms As Boolean) As Integer()

        Dim r, d As Dictionary(Of String, String)
        r = Nothing : d = Nothing
        Dim nonsynomical As Dictionary(Of String, Integer) = Nothing
        Call SetPropertyMakeIn(nonsynomical, r, d, name, readValues, defaultValues, nameToID, HasSynonyms)

        ReDim output(nonsynomical.Values.Max)
        For Each key As String In nonsynomical.Keys
            Dim v As Integer = ValueConverter.StrToInt(GetProperty(key, r, d), name, key)
            output(nonsynomical.Item(key)) = v
        Next key

        If HasSynonyms Then
            For i As Integer = 0 To UBound(output) Step 1
                If output(i) = 0 AndAlso nameToID.ContainsKey(i.ToString) Then
                    Dim source As Integer = CInt(nameToID.Item(i.ToString))
                    output(i) = output(source)
                End If
            Next i
        End If
        Return output
    End Function
    Private Sub SetPropertyMakeIn(ByRef nonsynomical As Dictionary(Of String, Integer), ByRef r As Dictionary(Of String, String), ByRef d As Dictionary(Of String, String), _
                                  ByRef name As String, ByRef readValues As Dictionary(Of String, String), _
                                  ByRef defaultValues As Dictionary(Of String, String), ByRef nameToID As Dictionary(Of String, Integer), _
                                  ByRef HasSynonyms As Boolean)
        r = PropertiesArrayToDictionary(GetProperty(name, readValues, defaultValues).ToUpper.Replace("=", " ").Split(CChar(";")))
        d = PropertiesArrayToDictionary(GetProperty(name, defaultValues, readValues).ToUpper.Replace("=", " ").Split(CChar(";")))

        If HasSynonyms Then
            nonsynomical = New Dictionary(Of String, Integer)
            For Each v As Integer In nameToID.Values
                If Not nonsynomical.ContainsKey(v.ToString) Then nonsynomical.Add(v.ToString, v)
            Next v
            r = ChangeToSynonymicNames(r, nameToID)
            d = ChangeToSynonymicNames(d, nameToID)
        Else
            nonsynomical = nameToID
        End If
    End Sub
    Private Function SetProperty(ByRef output As String, ByRef name As String, ByRef readValues As Dictionary(Of String, String), ByRef defaultValues As Dictionary(Of String, String)) As String
        output = ValueConverter.StrToDblStr(GetProperty(name, readValues, defaultValues))
        Return output
    End Function
    Private Function SetProperty(ByRef output() As String, ByRef name As String, ByRef readValues As Dictionary(Of String, String), ByRef defaultValues As Dictionary(Of String, String)) As String()
        output = GetProperty(name, readValues, defaultValues).ToUpper.Split(CChar(";"))
        Return output
    End Function
    Private Sub SetProperty(ByRef valueField As ClassFieldsHandler.GetFieldResult, _
                            ByRef readValues As Dictionary(Of String, String), ByRef defaultValues As Dictionary(Of String, String), _
                            ByRef nameToID As Dictionary(Of String, Integer), ByRef HasSynonyms As Boolean)

        If valueField.searchResultField.FieldType.FullName = GetType(String).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, SetProperty(CType(valueField.searchResult, String), _
                                                                                     valueField.searchResultField.Name, _
                                                                                     readValues, defaultValues))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(String()).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, SetProperty(CType(valueField.searchResult, String()), _
                                                                                     valueField.searchResultField.Name, _
                                                                                     readValues, defaultValues))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Double).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, SetProperty(CType(valueField.searchResult, Double), _
                                                                                     valueField.searchResultField.Name, _
                                                                                     readValues, defaultValues))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Double()).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, SetProperty(CType(valueField.searchResult, Double()), _
                                                                                     valueField.searchResultField.Name, _
                                                                                     readValues, defaultValues, nameToID, HasSynonyms))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Integer()).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, SetProperty(CType(valueField.searchResult, Integer()), _
                                                                                     valueField.searchResultField.Name, _
                                                                                     readValues, defaultValues, nameToID, HasSynonyms))
        Else
            Throw New Exception("Unexpected variable type")
        End If
    End Sub
#End Region

    Protected Friend Sub ParseRaces(ByRef outRaces As Dictionary(Of String, Integer), _
                                    ByRef outRaceNumberToRaceChar As Dictionary(Of Integer, String))
        Dim splitedRace() As String = ValueConverter.TxtSplit(Races)
        Dim srow() As String
        For Each item As String In splitedRace
            srow = item.Split(CChar(" "))
            For i As Integer = 0 To UBound(srow) Step 1
                If Not IsNothing(outRaces) Then outRaces.Add(srow(i).ToUpper, CInt(srow(UBound(srow))))
            Next i
            If Not IsNothing(outRaceNumberToRaceChar) Then outRaceNumberToRaceChar.Add(CInt(srow(UBound(srow))), srow(1).ToUpper)
        Next item
    End Sub
    Protected Friend Sub ParseItemTypes(ByRef outitemTypeID As Dictionary(Of String, Integer), _
                                        ByRef outitemType As Dictionary(Of Integer, String))
        Dim names() As String = [Enum].GetNames(GetType(GenDefaultValues.ItemTypes))
        For Each name In names
            Dim value As Integer = CType([Enum].Parse(GetType(GenDefaultValues.ItemTypes), name), GenDefaultValues.ItemTypes)
            If Not IsNothing(outitemType) Then outitemType.Add(value, name.ToUpper)
            If Not IsNothing(outitemTypeID) Then outitemTypeID.Add(name.ToUpper, value)
        Next name
    End Sub

    Enum TextLanguage
        Rus = 1
        Eng = 2
    End Enum

    ''' <summary>Игра использует такую строку для неиспользуемых юнитов, предметов и т.д.</summary>
    Public Const emptyItem As String = "G000000000"
    Public ReadOnly maxItemTypeID As Integer = GetType(GenDefaultValues.ItemTypes).GetEnumValues.Cast(Of Integer).Max

    'common
    Public ReadOnly defaultSigma As Double
    Public ReadOnly playableRaces As String()
    Public ReadOnly neutralRaces As String()
    Public ReadOnly generatorRaceToGameRace As Dictionary(Of String, String)
    Public ReadOnly generatorRaceToCapitalID As Dictionary(Of String, String)
    Public ReadOnly capitalToGeneratorRace As Dictionary(Of String, String)
    Public ReadOnly gameRaceToGeneratorRace As Dictionary(Of String, String)

    'units
    Public ReadOnly expBarDispersion As Double
    Public ReadOnly giantUnitsExpMultiplier As Double
    Public ReadOnly smallUnitsExpMultiplier As Double
    Public ReadOnly weakerUnitsRadius As Double

    'loot
    Public ReadOnly JewelItemsCostDevider As Double
    Public ReadOnly NonJewelItemsCostDevider As Double
    Public ReadOnly LootCostDispersion As Double
    Public ReadOnly Global_ItemType_ChanceMultiplier As String
    Public ReadOnly Global_AddedItem_ChanceMultiplier As Double
    Public ReadOnly Local_AddedItemType_ChanceMultiplier As Double()
    Public ReadOnly AddedItemTypeSearchRadius As Double
    Public ReadOnly SameItemsAmountRestriction As Integer()
    Public ReadOnly ThisBag_SameItems_ChanceMultiplier As Double()
    Public ReadOnly SameItemsTypeAmountRestriction As Integer()
    Public ReadOnly ThisBag_SameItemsType_ChanceMultiplier As Double()
    Public ReadOnly Local_SameItem_ChanceMultiplier As Double()
    Public ReadOnly CostBarExcessLimit As Double
    Public ReadOnly LootCostExcessLimit As Double
    Public ReadOnly MinRuinsLootCostMultiplier As Double
    Public ReadOnly CapitalLocationRadius As Double
    Public ReadOnly CapitalLocationBasicItems As String()

    'map
    Public ReadOnly minLocationRadiusAtAll As Double
    Public ReadOnly smallLocationRadius As Double
    Public ReadOnly LocRacesBlocks As String()
    Public ReadOnly StackRaceChance As Double()
    Public Const randomRaceID As Integer = -10001

#Region "Keywords"
    'ключевые слова
    Public Shared Function wMineTypeGold() As String
        Return My.Resources.mineTypeGold
    End Function
    Public Shared Function wMineTypeRandomMana() As String
        Return My.Resources.mineTypeRandomMana
    End Function
    Public Shared Function wMineTypeT1Mana() As String
        Return My.Resources.mineTypeT1Mana
    End Function
    Public Shared Function wMineTypeT3Mana() As String
        Return My.Resources.mineTypeT3Mana
    End Function
    Public Shared Function wObjKeyMage() As String
        Return My.Resources.objKeyMage
    End Function
    Public Shared Function wObjKeyMercenaries() As String
        Return My.Resources.objKeyMercenaries
    End Function
    Public Shared Function wObjKeyMerchant() As String
        Return My.Resources.objKeyMerchant
    End Function
    Public Shared Function wObjKeyMountain() As String
        Return My.Resources.objKeyMountain
    End Function
    Public Shared Function wObjKeyRuin() As String
        Return My.Resources.objKeyRuin
    End Function
    Public Shared Function wObjKeyTrainer() As String
        Return My.Resources.objKeyTrainer
    End Function
    Public Shared Function wObjKeyTown() As String
        Return My.Resources.objKeyTown
    End Function
    Public Shared Function wTemplate_CreationKeyword() As String
        Return My.Resources.template_creation
    End Function
    Public Shared Function wTemplate_LocationKeyword() As String
        Return My.Resources.template_location
    End Function
    Public Shared Function wTemplate_MapKeyword() As String
        Return My.Resources.template_map
    End Function
    Public Shared Function wTemplate_NewBlockKeyword() As String
        Return My.Resources.template_new_Block
    End Function
    Public Shared Function wTemplate_ReadFromFileKeyword() As String
        Return My.Resources.template_read_from_file
    End Function
    Public Shared Function wTemplate_RandomRaceShortKeyword() As String
        Return My.Resources.template_RandomRaceShortKeyword
    End Function
    Public Shared Function wTemplate_RandomRaceLongKeyword() As String
        Return My.Resources.template_RandomRaceLongKeyword
    End Function
#End Region

    Public Enum ItemTypes As Integer
        nonattack_artifact = 0
        relic = 1
        attack_artifact = 2
        banner = 3
        elixir = 4
        healing_elixir = 5
        ressurection_elixir = 6
        permanent_elixir = 7
        scroll = 8
        stuff = 9
        jewel = 10
        sphere = 11
        talisman = 12
        boots = 13
        special = 14
    End Enum

    Public Enum UnitAttackReach
        mage = 1
        archer = 2
        melee = 3
    End Enum

    Public Enum UnitClass
        fighter = 0
        archer = 1
        mage = 2
        support = 3
        special = 4
        leader = 5
        thief = 6
        summon = 7
        capitalGuard = 8
    End Enum

    Public Enum ExclusionState
        unknown = 0
        excluded = -1
        canUse = 1
    End Enum

#Region "Resources reading"
    'default files
    Private Function ReadResources(ByRef name As String, ByRef defaultValue As String, ByVal modSpecific As Boolean) As String
        Dim path As String = Environment.CurrentDirectory & "\Resources\"
        If modSpecific Then
            path &= My.Resources.modSettings_keyword & selectedMod & "\"
        Else
            path &= "common_game_data\"
        End If
        path &= name & ".txt"
        If IO.File.Exists(path) Then
            If Not IsNothing(myLog) Then
                Try
                    myLog.Add("Reading " & name & " from " & path)
                Catch ex As Exception
                    Console.WriteLine("Reading " & name & "; external; " & ex.Message)
                End Try
            End If
            Return String.Join(vbNewLine, Common.RecursiveReadFile(path))
        Else
            If Not IsNothing(myLog) Then
                Try
                    myLog.Add("Couldn't find " & path & " ; Reading " & name & " from internal resources")
                Catch ex As Exception
                    Console.WriteLine("Reading " & name & "; internal; " & ex.Message)
                End Try
            End If
            Return String.Join(vbNewLine, Common.RecursiveReadFile(defaultValue.Split(CChar(vbNewLine))))
        End If
    End Function

    'mod-specific
    Public Function BigStackUnits() As String
        Return ReadResources("BigStackUnits", "", True)
    End Function
    Public Function Constants() As String
        Return ReadResources("Constants", My.Resources.Constants, True)
    End Function
    Public Function ExcludedIDs() As String
        Return ReadResources("ExcludeIDs", "", True)
    End Function
    Public Function ExcludedIDs_ModLore() As String
        Return ReadResources("ExcludeIDs_ModLore", "", True)
    End Function
    Public Function LootItemChanceMultiplier() As String
        Return ReadResources("LootItemChanceMultiplier", "", True)
    End Function
    Public Function MapObjectRace() As String
        Return ReadResources("MapObjectRace", My.Resources.MapObjectRace, True)
    End Function
    Public Function ObjectsText(ByVal lang As GenDefaultValues.TextLanguage) As String
        Dim content As String = Nothing
        If lang = GenDefaultValues.TextLanguage.Rus Then
            content = ReadResources("ObjectsText_Rus", My.Resources.ObjectsText_Rus, True)
        ElseIf lang = GenDefaultValues.TextLanguage.Eng Then
            content = ReadResources("ObjectsText_Eng", My.Resources.ObjectsText_Eng, True)
        Else
            Throw New Exception("Unexpected language")
        End If
        Return content
    End Function
    Public Function PlateauConstructor() As String
        Return ReadResources("PlateauConstructor", My.Resources.PlateauConstructor, True)
    End Function
    Public Function PreservedItems() As String
        Return ReadResources("PreservedObjects", My.Resources.PreservedObjects, True)
    End Function
    Public Function Races() As String
        Return ReadResources("Races", My.Resources.Races, True)
    End Function
    Public Function RaceSublocations() As String
        Return ReadResources("RaceSublocations", My.Resources.RaceSublocations, True)
    End Function
    Public Function SingleUnits() As String
        Return ReadResources("SingleUnits", "", True)
    End Function
    Public Function UnitRace() As String
        Return ReadResources("UnitRace", "", True)
    End Function
    'common
    Public Function Capitals() As String
        Return ReadResources("Capitals", My.Resources.Capitals, False)
    End Function
    Public Function ExcludeIDsForNames() As String
        Return ReadResources("ExcludeIDsForNames", My.Resources.ExcludeIDsForNames, False)
    End Function
    Public Function Lords() As String
        Return ReadResources("Lords", My.Resources.Lords, False)
    End Function
    Public Function PlayableSubraces() As String
        Return ReadResources("PlayableSubraces", My.Resources.PlayableSubraces, False)
    End Function
    Public Function WaterBlocksCommon() As String
        Return ReadResources("WaterBlocksCommon", My.Resources.WaterBlocksCommon, False)
    End Function
    Public Function WaterBlocks3x3Objects() As String
        Return ReadResources("WaterBlocks3x3Objects", My.Resources.WaterBlocks3x3Objects, False)
    End Function
    Public Function WaterBlocksUnacceptable() As String
        Return ReadResources("WaterBlocksUnacceptable", My.Resources.WaterBlocksUnacceptable, False)
    End Function
#End Region

    Public Class MapObjectsText
        ''' <summary>ID лорда, список имен</summary>
        Private Lords As New Dictionary(Of String, TxTList)
        ''' <summary>ID столицы, список названий</summary>
        Private Capitals As New Dictionary(Of String, TxTList)
        ''' <summary>CityID@ID расы, список названий</summary>
        Private Cities As New Dictionary(Of String, TxTList)
        ''' <summary>ID руин, список названий</summary>
        Private Ruins As New Dictionary(Of String, TxTList)
        ''' <summary>ID башни мага, список названий</summary>
        Private Mages As New Dictionary(Of String, TxTList)
        ''' <summary>ID торговца, список названий</summary>
        Private Vendors As New Dictionary(Of String, TxTList)
        ''' <summary>ID торговца, список названий</summary>
        Private Mercenaries As New Dictionary(Of String, TxTList)
        ''' <summary>ID торговца, список названий</summary>
        Private Trainers As New Dictionary(Of String, TxTList)
        ''' <summary>ID типа объекта, описание</summary>
        Private Descriptions As New Dictionary(Of String, TxTList)

        Private rndVal As New RndValueGen
        Private genDefValues As GenDefaultValues

        Private Class TxTList
            Dim list As New List(Of String)
            Dim key As String
            Dim lang As GenDefaultValues.TextLanguage
            Dim genDefValues As GenDefaultValues
            Dim maxNameLength As Integer
            Dim readRepeat As Integer = -1

            Public Sub New(ByVal keyStr As String, ByVal maxNameL As Integer, ByVal langKey As GenDefaultValues.TextLanguage, ByRef g As GenDefaultValues)
                key = keyStr
                maxNameLength = maxNameL
                lang = langKey
                genDefValues = g
            End Sub
            Public Function GetRndItem(ByRef r As RndValueGen) As String
                If Count() = 0 Then Call Read()
                Dim i As Integer = r.RndInt(0, Count() - 1, True)
                Dim res As String = list.Item(i)
                If maxNameLength > -1 And readRepeat > 1 Then
                    Dim s As String = "_" & readRepeat
                    If res.Length + s.Length <= maxNameLength Then
                        res &= s
                    Else
                        res = res.Substring(0, readRepeat).ToUpper & res.Substring(readRepeat)
                    End If
                End If
                list.RemoveAt(i)
                Return res
            End Function
            Public Sub AddItem(ByRef v As String)
                list.Add(v)
            End Sub
            Public Sub Read()
                Dim content() As String = ValueConverter.TxtSplit(genDefValues.ObjectsText(lang))
                For Each s As String In content
                    Dim splited() As String = s.Split(CChar("_"))
                    If splited(0).ToUpper.StartsWith(key.ToUpper) Then
                        For j As Integer = 1 To UBound(splited) Step 1
                            Call AddItem(splited(j).Trim(CChar(" "), CChar(vbTab)))
                        Next j
                    End If
                Next s
                readRepeat += 1
            End Sub
            Public Sub Clear()
                list.Clear()
            End Sub
            Public Function Count() As Integer
                Return list.Count
            End Function
            Public Function ToArray() As String()
                Return list.ToArray
            End Function
        End Class

        Private objectsWithDescriptions() As String = {"G000SI0000MAGE", "G000SI0000MERH", "G000SI0000MERC", "G000SI0000TRAI"}
        Private key() As String = {"G000LR", "G000FT0000NE@", "G000FT", "G000RU"}
        Private Const descriptionKey As String = "D"

        Private maxNameLen() As Integer = {SetName.LordNameMaxLen, _
                                           SetName.CityNameMaxLen, _
                                           SetName.CapitalNameMaxLen, _
                                           SetName.RuinsNameMaxLen, _
                                           SetName.MerchantNameMaxLen, _
                                           SetName.MerchantNameMaxLen, _
                                           SetName.MerchantNameMaxLen, _
                                           SetName.MerchantNameMaxLen}

        Public Sub New(ByRef lang As GenDefaultValues.TextLanguage, ByRef g As GenDefaultValues)
            genDefValues = g
            ReDim Preserve key(UBound(key) + objectsWithDescriptions.Length)
            For i As Integer = 0 To UBound(objectsWithDescriptions) Step 1
                key(UBound(key) - UBound(objectsWithDescriptions) + i) = objectsWithDescriptions(i)
            Next i
            Call ReadFields(lang)
            Call PrintValuesCount()
        End Sub

        Private Function GetValue(ByRef list As Dictionary(Of String, TxTList), ByRef id As String) As String
            If list.ContainsKey(id.ToUpper) Then
                Return list.Item(id.ToUpper).GetRndItem(rndVal)
            Else
                Return id & "_name"
            End If
        End Function
        Private Function GetArray(ByRef list As Dictionary(Of String, TxTList), ByRef id As String) As String()
            If list.ContainsKey(id.ToUpper) Then
                Return list.Item(id.ToUpper).ToArray
            Else
                Return {id & "_name"}
            End If
        End Function
        Public Function GetDescription(ByRef ID As String) As String
            For Each v As String In objectsWithDescriptions
                If ID.ToUpper.StartsWith(v.ToUpper) Then Return GetValue(Descriptions, descriptionKey & v.ToUpper)
            Next v
            Return ID & "_description"
        End Function
        Public Function DescriptionsArray(ByRef ID As String) As String()
            For Each v As String In objectsWithDescriptions
                If ID.ToUpper.StartsWith(v.ToUpper) Then Return GetArray(Descriptions, descriptionKey & v.ToUpper)
            Next v
            Return {ID & "_description"}
        End Function
        Public Function GetLordName(ByRef ID As String) As String
            Return GetValue(Lords, ID)
        End Function
        Public Function LordNamesArray(ByRef ID As String) As String()
            Return GetArray(Lords, ID)
        End Function
        Public Function GetCityName(ByRef ID As String) As String
            Return GetValue(Cities, ID)
        End Function
        Public Function CityNamesArray(ByRef ID As String) As String()
            Return GetArray(Cities, ID)
        End Function
        Public Function GetCapitalName(ByRef ID As String) As String
            Return GetValue(Capitals, ID)
        End Function
        Public Function CapitalNamesArray(ByRef ID As String) As String()
            Return GetArray(Capitals, ID)
        End Function
        Public Function GetRuinName(ByRef ID As String) As String
            Return GetValue(Ruins, ID)
        End Function
        Public Function RuinNamesArray(ByRef ID As String) As String()
            Return GetArray(Ruins, ID)
        End Function
        Public Function GetMageName(ByRef ID As String) As String
            Return GetValue(Mages, ID)
        End Function
        Public Function MageNamesArray(ByRef ID As String) As String()
            Return GetArray(Mages, ID)
        End Function
        Public Function GetMercenaryName(ByRef ID As String) As String
            Return GetValue(Mercenaries, ID)
        End Function
        Public Function MercenaryNamesArray(ByRef ID As String) As String()
            Return GetArray(Mercenaries, ID)
        End Function
        Public Function GetVendorName(ByRef ID As String) As String
            Return GetValue(Vendors, ID)
        End Function
        Public Function VendorNamesArray(ByRef ID As String) As String()
            Return GetArray(Vendors, ID)
        End Function
        Public Function GetTrainerName(ByRef ID As String) As String
            Return GetValue(Trainers, ID)
        End Function
        Public Function TrainerNamesArray(ByRef ID As String) As String()
            Return GetArray(Trainers, ID)
        End Function

        Private Sub ReadFields(ByRef lang As GenDefaultValues.TextLanguage)
            Dim d() As Dictionary(Of String, TxTList) = ToArray()
            For i As Integer = 0 To UBound(d) Step 1
                If IsNothing(d(i)) Then
                    d(i) = New Dictionary(Of String, TxTList)()
                Else
                    d(i).Clear()
                End If
            Next i
            Dim content() As String = ValueConverter.TxtSplit(genDefValues.ObjectsText(lang))
            For Each s As String In content
                Dim splited() As String = s.Split(CChar("_"))
                For i As Integer = 0 To UBound(key) Step 1
                    If splited(0).ToUpper.StartsWith(key(i).ToUpper) Then
                        If Not d(i).ContainsKey(splited(0).ToUpper) Then d(i).Add(splited(0).ToUpper, New TxTList(splited(0).ToUpper, maxNameLen(i), lang, genDefValues))
                        Exit For
                    End If
                Next i
            Next s
            For i As Integer = 0 To UBound(d) Step 1
                For Each f As TxTList In d(i).Values
                    Call f.Read()
                Next f
            Next i
            Call ToFields(d)
            For i As Integer = 0 To UBound(objectsWithDescriptions) Step 1
                Dim k As String = descriptionKey & objectsWithDescriptions(i).ToUpper
                Descriptions.Add(k, New TxTList(k, -1, lang, genDefValues))
                Call Descriptions.Item(k).Read()
            Next i

            Dim allNames, allDescriptions As New List(Of String)
            For i As Integer = 0 To UBound(d) Step 1
                For Each k As String In d(i).Keys
                    For Each v As String In d(i).Item(k).ToArray
                        If v.Length > maxNameLen(i) Then
                            Throw New Exception("Name of " & k & " is too long: " & v)
                        ElseIf allNames.Contains(v.ToUpper) Then
                            If Not k.ToUpper.StartsWith(key(0).ToUpper) Then
                                Throw New Exception("Duplicated name: " & v)
                            End If
                        Else
                            If Not v.Substring(0, 1) = v.Substring(0, 1).ToUpper Then
                                Throw New Exception("Name must start with upper case: " & v)
                            End If
                            allNames.Add(v.ToUpper)
                        End If
                    Next v
                Next k
            Next i
            For Each k As String In Descriptions.Keys
                For Each v As String In GetArray(Descriptions, k)
                    If v.Length > SetName.DescriptionMaxLen Then
                        Throw New Exception("Description of " & k & " is too long: " & v)
                    ElseIf allDescriptions.Contains(v.ToUpper) Then
                        Throw New Exception("Duplicated description: " & v)
                    Else
                        If Not v.Substring(0, 1) = v.Substring(0, 1).ToUpper Then
                            Throw New Exception("Description must start with upper case: " & v)
                        End If
                        allDescriptions.Add(v.ToUpper)
                    End If
                Next v
            Next k
        End Sub
        Private Sub PrintValuesCount()
            Dim d() As Dictionary(Of String, TxTList) = ToArray()
            Dim delimiter As String = "-----------------------"
            Console.WriteLine(delimiter)
            For i As Integer = 0 To UBound(d) Step 1
                For Each k As String In d(i).Keys
                    Console.WriteLine(k & vbTab & d(i).Item(k).Count)
                Next k
                Console.WriteLine(delimiter)
            Next i
            For Each k As String In Descriptions.Keys
                Console.WriteLine(k & vbTab & Descriptions.Item(k).Count)
            Next k
            Console.WriteLine(delimiter)
        End Sub
        Public Sub Clear()
            Dim d() As Dictionary(Of String, TxTList) = ToArray()
            For i As Integer = 0 To UBound(d) Step 1
                If IsNothing(d(i)) Then
                    For Each k As String In d(i).Keys
                        d(i).Item(k).Clear()
                    Next k
                End If
            Next i
            Call ToFields(d)
        End Sub

        Private Function ToArray() As Dictionary(Of String, TxTList)()
            Return {Lords, Cities, Capitals, Ruins, Mages, Vendors, Mercenaries, Trainers}
        End Function
        Private Sub ToFields(ByRef d() As Dictionary(Of String, TxTList))
            Lords = d(0)
            Cities = d(1)
            Capitals = d(2)
            Ruins = d(3)
            Mages = d(4)
            Vendors = d(5)
            Mercenaries = d(6)
            Trainers = d(7)
        End Sub
    End Class
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
    Public Sub Add(ByRef contString As AllDataStructues.DesiredStats)
        If Not Enabled Then Exit Sub
        Content.Add(AllDataStructues.DesiredStats.Print(contString, comm.defValues.RaceNumberToRaceChar))
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
    Public Sub MAdd(ByVal LogID As Integer, ByVal contString As AllDataStructues.DesiredStats)
        If Not Enabled Then Exit Sub
        multiThreadLog(LogID).Add(AllDataStructues.DesiredStats.Print(contString, comm.defValues.RaceNumberToRaceChar))
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

Public Class ClassFieldsHandler

    Public Const defaultSearchFieldsSettings As Reflection.BindingFlags = Reflection.BindingFlags.Public Or Reflection.BindingFlags.Instance Or Reflection.BindingFlags.IgnoreCase
    Public Const subfieldsDelimiter As Char = CChar(".")

    Public Class GetFieldResult
        ''' <summary>Найденная информация о поле</summary>
        Public searchResultField As Reflection.FieldInfo
        ''' <summary>Экземпляр объекта поля</summary>
        Public searchResult As Object
        ''' <summary>Экземпляры цепочки объектов, в последнем из которых содержится искомый объект</summary>
        Public parents() As Object
        ''' <summary>Если возникла ошибка, то она будет доступна здесь</summary>
        Public throwedExcetion As Exception

        Public Function LastParent() As Object
            If Not IsNothing(parents) Then
                Return parents(UBound(parents))
            Else
                Return Nothing
            End If
        End Function
    End Class

    ''' <summary>Попытается найти поле в объекте и задать ему значение.</summary>
    ''' <param name="parent">Обеъкт, в котором ищем поле</param>
    ''' <param name="fieldValue">Задаваемое значение</param>
    ''' <param name="fieldName">
    ''' Имя поля.
    ''' Через точку можно получить доступ к вложенным объектами.
    ''' Пример:
    ''' parent.childA.childB.childC.fieldName</param>
    Public Shared Sub SetFieldValue(parent As Object, ByVal fieldValue As Object, ByVal fieldName As String)
        Dim info As GetFieldResult = GetField(parent, fieldName)
        If IsNothing(fieldValue) Then
            info.searchResultField.SetValue(info.LastParent, Nothing)
        ElseIf Not IsNothing(info.searchResult) Then
            Dim T As Type = info.searchResultField.FieldType
            info.searchResultField.SetValue(info.LastParent, CTypeDynamic(fieldValue, T))
        Else
            info.searchResultField.SetValue(info.LastParent, fieldValue)
        End If
    End Sub
    ''' <summary>Попытается найти поле в объекте и получить его значение.</summary>
    ''' <param name="parent">Обеъкт, в котором ищем поле</param>
    ''' <param name="fieldName">
    ''' Имя поля.
    ''' Через точку можно получить доступ к вложенным объектами.
    ''' Пример:
    ''' parent.childA.childB.childC.fieldName</param>
    Public Shared Function GetFieldValue(parent As Object, ByVal fieldName As String) As Object
        Dim info As GetFieldResult = GetField(parent, fieldName)
        If Not IsNothing(info.searchResult) Then
            Return info.searchResultField.GetValue(info.parents(UBound(info.parents)))
        Else
            Return Nothing
        End If
    End Function
    ''' <summary>Попытается найти поле в объекте и получить его значение.</summary>
    ''' <param name="parent">Обеъкт, в котором ищем поле</param>
    ''' <param name="fieldName">
    ''' Имя поля.
    ''' Через точку можно получить доступ к вложенным объектами.
    ''' Пример:
    ''' parent.childA.childB.childC.fieldName</param>
    ''' <param name="searchSettings">Настройки поиска</param>
    Public Shared Function GetField(parent As Object, ByVal fieldName As String, Optional ByVal searchSettings As Reflection.BindingFlags = defaultSearchFieldsSettings) As GetFieldResult
        Try
            Dim s() As String = fieldName.Split(subfieldsDelimiter)
            Dim res As New GetFieldResult
            ReDim res.parents(UBound(s))
            Dim fields(UBound(s)) As Reflection.FieldInfo
            For i As Integer = 0 To UBound(s) Step 1
                If i = 0 Then
                    fields(i) = parent.GetType().GetField(s(i), searchSettings)
                    res.parents(i) = parent
                    If i < UBound(s) Then
                        res.parents(i + 1) = fields(i).GetValue(parent)
                    End If
                Else
                    fields(i) = res.parents(i).GetType().GetField(s(i), searchSettings)
                    If i < UBound(s) Then
                        res.parents(i + 1) = fields(i).GetValue(res.parents(i))
                    End If
                End If
            Next i
            res.searchResultField = fields(UBound(fields))
            res.searchResult = fields(UBound(s)).GetValue(res.parents(UBound(s)))
            Return res
        Catch ex As Exception
            Return New GetFieldResult With {.parents = Nothing, .searchResult = Nothing, .searchResultField = Nothing, .throwedExcetion = ex}
        End Try
    End Function

    ''' <summary>Вернет имена полей в объекте.</summary>
    ''' <param name="parent">Обеъкт, в котором ищем поля</param>
    ''' <param name="Ignore">Имена полей, которые не попадут в список. Регистр игнорируется</param>
    ''' <param name="searchSettings">Настройки поиска</param>
    Public Shared Function GetFieldsNamesList(parent As Object, Optional ByRef Ignore() As String = Nothing, Optional ByVal searchSettings As Reflection.BindingFlags = defaultSearchFieldsSettings) As String()
        If IsNothing(parent) Then Return Nothing
        Dim all() As Reflection.FieldInfo = GetFieldsList(parent, Ignore, searchSettings)
        Dim result(UBound(all)) As String
        For i As Integer = 0 To UBound(all) Step 1
            result(i) = all(i).Name
        Next i
        Return result
    End Function
    ''' <summary>Вернет поля в объекте.</summary>
    ''' <param name="parent">Обеъкт, в котором ищем поля</param>
    ''' <param name="Ignore">Имена полей, которые не попадут в список. Регистр игнорируется</param>
    ''' <param name="searchSettings">Настройки поиска</param>
    Public Shared Function GetFieldsList(parent As Object, Optional ByRef Ignore() As String = Nothing, Optional ByVal searchSettings As Reflection.BindingFlags = defaultSearchFieldsSettings) As Reflection.FieldInfo()
        If IsNothing(parent) Then Return Nothing
        Dim all() As Reflection.FieldInfo = parent.GetType.GetFields(searchSettings)
        Dim result(UBound(all)) As Reflection.FieldInfo
        Dim n As Integer = -1
        Dim IList As List(Of String) = Nothing
        If Not IsNothing(Ignore) Then IList = ToUpperedList(Ignore)
        For Each item As Reflection.FieldInfo In all
            If IsNothing(IList) OrElse Not IList.Contains(item.Name.ToUpper) Then
                n += 1
                result(n) = item
            End If
        Next item
        If n < UBound(result) Then ReDim Preserve result(n)
        Return result
    End Function

    Public Shared Function GetObjectSubfields(ByRef obj As Object, _
                                              Optional ByVal searchSettings As Reflection.BindingFlags = defaultSearchFieldsSettings) As String()
        Dim fields() As String = GetFieldsNamesList(obj)
        Dim fullList As New List(Of String)
        For Each f As String In fields
            Call GetObjectSubfields(fullList, GetField(obj, f), "", searchSettings)
        Next f
        Return fullList.ToArray()
    End Function
    Private Shared Sub GetObjectSubfields(ByRef output As List(Of String), _
                                          ByRef obj As GetFieldResult, _
                                          ByVal parentsChain As String, _
                                          Optional ByVal searchSettings As Reflection.BindingFlags = defaultSearchFieldsSettings)
        Dim fields() As String = GetFieldsNamesList(obj.searchResult, Nothing, searchSettings)
        If IsNothing(fields) OrElse fields.Length = 0 OrElse TypeOf obj.searchResult Is [Enum] Then
            output.Add(parentsChain & obj.searchResultField.Name)
        Else
            Dim f As GetFieldResult
            For Each fname As String In fields
                f = GetField(obj.searchResult, fname, searchSettings)
                Call GetObjectSubfields(output, f, parentsChain & obj.searchResultField.Name & subfieldsDelimiter, searchSettings)
            Next fname
        End If
    End Sub

    Private Shared Function ToUpperedList(ByRef a() As String) As List(Of String)
        Dim IList As New List(Of String)
        For Each item As String In a
            IList.Add(item.ToUpper)
        Next item
        Return IList
    End Function
End Class

