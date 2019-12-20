Imports System.Threading.Tasks

Public Class RandStack

    Private emptyItem As String = "G000000000"

    Private busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
    Private firstrow() As Integer = New Integer() {0, 2, 4}
    Private secondrow() As Integer = New Integer() {1, 3, 5}

    Public Structure Stack
        ''' <summary>ID юнита для каждой позиции</summary>
        Dim pos() As String
        ''' <summary>В какой позиции находится лидер</summary>
        Dim leaderPos As Integer
        ''' <summary>Предметы отряда. GxxxIGxxxx</summary>
        Dim items As List(Of String)
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
                                  .unitCost = Cost.Copy(v.unitCost)}
        End Function
    End Structure

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
        ''' <summary> Идентификатор локации, для которой сгенерирован отряд</summary>
        Dim LocationName As String

        Public Shared Function Copy(ByVal v As DesiredStats) As DesiredStats
            Dim RacesList As New List(Of Integer)
            For Each Item As Integer In v.Race
                RacesList.Add(Item)
            Next Item
            Return New DesiredStats With {.ExpBarAverage = v.ExpBarAverage, _
                                          .ExpStackKilled = v.ExpStackKilled, _
                                          .MaxGiants = v.MaxGiants, _
                                          .MeleeCount = v.MeleeCount, _
                                          .Race = RacesList, _
                                          .StackSize = v.StackSize, _
                                          .LootCost = v.LootCost, _
                                          .LocationName = v.LocationName}
        End Function
        ''' <param name="RaceNumberToRaceChar">Преобразует номер расы в ее текстовый идентификатор. Если передать Nothing, то будут печататься номера рас</param>
        Public Shared Function Print(ByVal v As DesiredStats, ByRef RaceNumberToRaceChar As Dictionary(Of Integer, String)) As String
            Dim races As String = ""
            For Each Item As Integer In v.Race
                If Not races = "" Then races &= "+"
                If Not IsNothing(RaceNumberToRaceChar) Then
                    races &= RaceNumberToRaceChar.Item(Item)
                Else
                    races &= Item
                End If
            Next Item
            Return "ID" & vbTab & v.LocationName & vbNewLine & _
                   "AverageExpBar" & vbTab & v.ExpBarAverage & vbNewLine & _
                   "ExpStackKilled" & vbTab & v.ExpStackKilled & vbNewLine & _
                   "Race" & vbTab & races & vbNewLine & _
                   "StackSize" & vbTab & v.StackSize & vbNewLine & _
                   "MaxGiants" & vbTab & v.MaxGiants & vbNewLine & _
                   "MeleeCount" & vbTab & v.MeleeCount & vbNewLine & _
                   "LootCost" & vbTab & v.LootCost & vbNewLine
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
                    res.Gold = CInt(v)
                ElseIf s1 = "r" Then
                    res.Red = CInt(v)
                ElseIf s1 = "y" Then
                    res.Blue = CInt(v)
                ElseIf s1 = "e" Then
                    res.Black = CInt(v)
                ElseIf s1 = "w" Then
                    res.White = CInt(v)
                ElseIf s1 = "b" Then
                    res.Green = CInt(v)
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
    End Structure

    Public Structure Item
        ''' <summary>Название</summary>
        Dim name As String
        ''' <summary>GxxxIGxxxx</summary>
        Dim itemID As String
        ''' <summary>Описание типов в ./Resources/Items.txt</summary>
        Dim Type As Integer
        ''' <summary>Цена покупки предмета. При продаже цена в пять раз меньше</summary>
        Dim itemCost As Cost

        Public Shared Function Copy(ByVal v As Item) As Item
            Return New Item With {.name = v.name, _
                                  .itemID = v.itemID, _
                                  .Type = v.Type, _
                                  .itemCost = Cost.Copy(v.itemCost)}
        End Function
    End Structure

    Private AllLeaders(), AllFighters(), ExcludedUnits() As Unit
    Private MagicItem(), ExcludedItems() As Item
    Public serialExecution As Boolean
    Public rndgen As RndValueGen
    Public comm As New Common

    Private ExpBarLeaders(), ExpBarFighters(), ExpKilledLeaders(), ExpKilledFighters(), multLeaders(), multFighters() As Double
    Private ItemGoldCost(), multItems() As Double
    Private minItemGoldCost As Integer
    Private itemType As New Dictionary(Of Integer, String)

    ''' <param name="AllUnitsList">Dсе юниты в игре</param>
    ''' <param name="AllItemsList">Все предметы в игре</param>
    ''' <param name="ExcludeLists">Файлы со списками исключенных объектов. Записи в них могут повторяться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default%</param>
    ''' <param name="serial">True, если код, использующий генератор выполняется в одном потоке</param>
    Public Sub New(ByRef AllUnitsList() As Unit, ByRef AllItemsList() As Item, ByRef ExcludeLists() As String, ByRef serial As Boolean)
        serialExecution = serial
        rndgen = comm.rndgen
        If IsNothing(AllUnitsList) Or IsNothing(AllItemsList) Then Exit Sub

        Call comm.ReadExcludedObjectsList(ExcludeLists)

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

        Dim splitedUnitRace() As String = comm.TxtSplit(My.Resources.UnitRace)
        Dim srow() As String
        Dim customRace As New Dictionary(Of String, String)
        For i As Integer = 0 To UBound(splitedUnitRace) Step 1
            srow = splitedUnitRace(i).Split(CChar(" "))
            customRace.Add(srow(0).ToUpper, srow(2).ToUpper)
        Next i
        Call MakeAccessoryArrays(AllUnitsList, customRace, AllFighters, cat, 1, ExpBarFighters, ExpKilledFighters, multFighters)
        Call MakeAccessoryArrays(AllUnitsList, customRace, AllLeaders, cat, 2, ExpBarLeaders, ExpKilledLeaders, multLeaders)
        Call MakeAccessoryArrays(AllUnitsList, customRace, ExcludedUnits, cat, 0, Nothing, Nothing, Nothing)

        Dim splitedItemsTypes() As String = comm.TxtSplit(My.Resources.Items)
        For i As Integer = 0 To UBound(splitedItemsTypes) Step 1
            srow = splitedItemsTypes(i).Split(CChar(" "))
            itemType.Add(CInt(srow(0)), srow(1).ToUpper)
        Next i
        Call MakeAccessoryArrays(AllItemsList, MagicItem, ExcludedItems, ItemGoldCost, multItems)

    End Sub
    Private Sub MakeAccessoryArrays(ByRef allunits() As Unit, ByRef customRace As Dictionary(Of String, String), ByRef units() As Unit, _
                                    ByRef cat() As Integer, ByRef addcat As Integer, _
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
                units(n) = Unit.Copy(allunits(i))
                If customRace.ContainsKey(units(n).unitID) Then
                    units(n).race = comm.RaceIdentifierToSubrace(customRace.Item(units(n).unitID))
                Else
                    units(n).race = comm.RaceIdentifierToSubrace(units(n).race)
                End If
                If Not IsNothing(expBar) Then expBar(n) = units(n).EXPnext
                If Not IsNothing(expKuilled) Then expKuilled(n) = units(n).EXPkilled
                If Not IsNothing(mult) Then
                    If units(n).small Then
                        mult(n) = CDbl(My.Resources.smallUnitsExpMultiplicator)
                    Else
                        mult(n) = CDbl(My.Resources.giantUnitsExpMultiplicator)
                    End If
                End If
            End If
        Next i
    End Sub
    Private Sub MakeAccessoryArrays(ByRef allitems() As Item, ByRef items() As Item, ByRef excluded() As Item, _
                                    ByRef GoldCost() As Double, ByRef mult() As Double)
        Dim n As Integer = -1
        Dim m As Integer = -1
        Dim add(UBound(allitems)) As Boolean
        For i As Integer = 0 To UBound(allitems) Step 1
            If Not comm.excludedObjects.Contains(allitems(i).itemID.ToUpper) _
            And Not comm.excludedObjects.Contains(itemType.Item(allitems(i).Type)) _
            And allitems(i).itemCost.Gold > 0 Then
                add(i) = True
                n += 1
            Else
                m += 1
            End If
        Next i
        ReDim items(n), GoldCost(n), multItems(n), excluded(m)

        minItemGoldCost = Integer.MaxValue
        n = -1
        m = -1
        For i As Integer = 0 To UBound(allitems) Step 1
            If add(i) Then
                n += 1
                items(n) = Item.Copy(allitems(i))
                GoldCost(n) = items(n).itemCost.Gold
                If itemType.Item(items(n).Type) = "JEWEL" Then
                    mult(n) = CDbl(My.Resources.JewelItemsCostMultiplicator)
                Else
                    mult(n) = CDbl(My.Resources.nonJewelItemsCostMultiplicator)
                End If
                If minItemGoldCost * mult(n) > items(n).itemCost.Gold Then
                    minItemGoldCost = CInt(items(n).itemCost.Gold / mult(n))
                End If
            Else
                m += 1
                excluded(m) = Item.Copy(allitems(i))
            End If
        Next i
    End Sub

    ''' <summary>Найдет статы юнита по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxUUxxxx</param>
    Public Function FindUnitStats(ByRef ID As String) As Unit
        Dim f As String = ID.ToUpper
        Dim a()() As Unit = New Unit()() {AllFighters, AllLeaders, ExcludedUnits}
        For u As Integer = 0 To UBound(a) Step 1
            If Not IsNothing(a(u)) Then
                For i As Integer = 0 To UBound(a(u)) Step 1
                    If f = a(u)(i).unitID Then Return Unit.Copy(a(u)(i))
                Next i
            End If
        Next u
        Return Nothing
    End Function
    ''' <summary>Найдет статы предмета по ID (нечувствительно к регистру)</summary>
    ''' <param name="ID">GxxxIGxxxx</param>
    Public Function FindItemStats(ByRef ID As String) As Item
        Dim f As String = ID.ToUpper
        Dim a()() As Item = New Item()() {MagicItem, ExcludedItems}
        For u As Integer = 0 To UBound(a) Step 1
            If Not IsNothing(a(u)) Then
                For i As Integer = 0 To UBound(a(u)) Step 1
                    If f = a(u)(i).itemID Then Return Item.Copy(a(u)(i))
                Next i
            End If
        Next u
        Return Nothing
    End Function

    ''' <summary>Вычисляет параметры отряда по составу</summary>
    ''' <param name="s">ID юнитов и предметов отряда</param>
    Public Function StackStats(ByRef s As Stack) As DesiredStats
        Dim result As New DesiredStats With {.Race = New List(Of Integer)}
        Dim u As Unit
        Dim m As Item
        For i As Integer = 0 To UBound(s.pos) Step 1
            If Not s.pos(i).ToUpper = emptyItem Then
                u = FindUnitStats(s.pos(i))
                If u.unitID = "" Then Throw New Exception("Неизвестный id юнита: " & s.pos(i))
                If Not result.Race.Contains(u.race) Then result.Race.Add(u.race)
                result.ExpStackKilled += u.EXPkilled
                result.ExpBarAverage += u.EXPnext
                If u.small Then
                    result.StackSize += 1
                Else
                    result.StackSize += 2
                    result.MaxGiants += 1
                End If
                If Not u.small Or u.reach = 3 Then result.MeleeCount += 1
            End If
        Next i
        result.ExpBarAverage = CInt(result.ExpBarAverage / result.StackSize)
        For Each Item As String In s.items
            m = FindItemStats(Item)
            If m.itemID = "" Then Throw New Exception("Неизвестный id предмета: " & Item)
            If itemType.Item(m.Type) = "JEWEL" Then
                result.LootCost = CInt(result.LootCost + m.itemCost.Gold / CDbl(My.Resources.JewelItemsCostMultiplicator))
            Else
                result.LootCost = CInt(result.LootCost + m.itemCost.Gold / CDbl(My.Resources.nonJewelItemsCostMultiplicator))
            End If
        Next Item
        Return result
    End Function

    ''' <summary>Генерирует набор предметов</summary>
    ''' <param name="GoldCost">Максимальная стоимость набора в золоте. Драгоценности считаются дешевле в два раза</param>
    Public Function ItemsGen(ByRef GoldCost As Integer) As List(Of String)
        Dim costBar, maxCost, selected As Integer
        Dim DynCost As Integer = GoldCost
        Dim IDs As New List(Of Integer)
        Dim result As New List(Of String)
        Do While DynCost >= minItemGoldCost
            costBar = CInt(rndgen.Rand(CDbl(minItemGoldCost), CDbl(DynCost), serialExecution))
            maxCost = Math.Min(2 * costBar, DynCost)
            IDs.Clear()
            For i As Integer = 0 To UBound(MagicItem) Step 1
                If MagicItem(i).itemCost.Gold <= maxCost * multItems(i) Then IDs.Add(i)
            Next i
            selected = comm.RandomSelection(IDs, New Double()() {ItemGoldCost}, New Double() {costBar}, multItems, serialExecution)
            result.Add(MagicItem(selected).itemID)
            DynCost = CInt(DynCost - MagicItem(selected).itemCost.Gold / multItems(selected))
        Loop
        Return result
    End Function

    ''' <summary>Затычка: вернет отряд из двух сквайров и трех лучников. Лидер - паладин. С зельем воскрешения</summary>
    Public Function GenGag() As Stack
        Dim result As New Stack
        ReDim result.pos(UBound(busytransfer))
        Dim fighter1 As String = "G000UU0001"
        Dim fighter2 As String = "G000UU0006"
        Dim leader As String = "G000UU5356"
        For i As Integer = 0 To UBound(firstrow) Step 1
            result.pos(firstrow(i)) = fighter1
        Next i
        For i As Integer = 0 To UBound(secondrow) Step 1
            result.pos(secondrow(i)) = fighter2
        Next i
        result.pos(firstrow(2)) = leader
        result.leaderPos = firstrow(2)
        result.items = New List(Of String)
        result.items.Add("G000IG0001")
        Return result
    End Function

    ''' <param name="StackStats">Желаемые параметры стэка</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров</param>
    Public Function Gen(ByRef StackStats As DesiredStats, ByRef GroundTile As Boolean) As Stack

        Dim DynStackStats As DesiredStats = DesiredStats.Copy(StackStats)
        DynStackStats.Race.Clear()
        For Each i As Integer In StackStats.Race
            Dim s As Integer = comm.RaceIdentifierToSubrace(i)
            If Not DynStackStats.Race.Contains(s) Then DynStackStats.Race.Add(s)
        Next i
        Dim PossibleLeaders, SelectedFighters As New List(Of Integer)

        'создаем список лидеров, которых вообще можем использовать
        PossibleLeaders.Clear()
        Dim Tolerance As Double = 0
        Do While PossibleLeaders.Count = 0
            Tolerance += 0.1
            For i As Integer = 0 To UBound(AllLeaders) Step 1
                If SelectPossibleLeader(i, Tolerance, DynStackStats, GroundTile) Then PossibleLeaders.Add(i)
            Next i

            If Tolerance * DynStackStats.ExpBarAverage > 10000 And Tolerance * DynStackStats.ExpStackKilled > 10000 Then
                If DynStackStats.MaxGiants < 1 Then
                    DynStackStats.MaxGiants = 1
                    Tolerance = 0
                Else
                    Throw New Exception("Что-то не так в выборе возможных лидеров отряда" & vbNewLine & _
                                        "Имя локации: " & StackStats.LocationName & vbNewLine & _
                                        "StackStats:" & vbNewLine & DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                        "DynStackStats:" & vbNewLine & DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
                End If
            End If
        Loop

        Dim SelectedLeader As Integer = comm.RandomSelection(PossibleLeaders, {ExpBarLeaders}, {DynStackStats.ExpBarAverage}, multLeaders, serialExecution)

        If SelectedLeader = -1 Then
            Throw New Exception("Возможно, бесконечный цикл в случайном выборе из массива возможных лидеров" & vbNewLine & _
                                "Имя локации: " & StackStats.LocationName & vbNewLine & _
                                "StackStats:" & vbNewLine & DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                "DynStackStats:" & vbNewLine & DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
        End If
        Dim FreeMeleeSlots As Integer = 3

        'теперь нужно добрать воинов в отряд
        Dim R As Double = rndgen.Rand(0, 1, serialExecution)
        If R < 0.1 Then
            DynStackStats.StackSize -= 1
        ElseIf R > 0.9 Then
            DynStackStats.StackSize += 1
            If DynStackStats.StackSize - DynStackStats.MeleeCount < secondrow.Length Then DynStackStats.MeleeCount += 1
        End If
        DynStackStats.StackSize = Math.Min(Math.Min(DynStackStats.StackSize, AllLeaders(SelectedLeader).leadership), 6)
        If AllLeaders(SelectedLeader).small Then
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 1)
        Else
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 2)
        End If
        DynStackStats.MeleeCount = Math.Min(DynStackStats.MeleeCount, 3)
        DynStackStats.MaxGiants = Math.Min(DynStackStats.MaxGiants, 3)

        Call ChangeLimit(AllLeaders, SelectedLeader, DynStackStats, FreeMeleeSlots)

        Dim fighter As Integer
        Do While DynStackStats.StackSize > 0
            'создаем список воинов, которых можно использовать
            fighter = SelectFighters(False, False, DynStackStats, FreeMeleeSlots)
            If fighter = -1 Then
                fighter = SelectFighters(True, False, DynStackStats, FreeMeleeSlots)
                If fighter = -1 Then fighter = SelectFighters(True, True, DynStackStats, FreeMeleeSlots)
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
                                    "StackStats:" & vbNewLine & DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                    "DynStackStats:" & vbNewLine & DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
            Else
                SelectedFighters.Add(fighter)
            End If
        Loop
        'в итоге должны получить лидера и остальной отряд
        'дальше расставляем в зависимости от размера и дальности атаки и пишем в файл карты
        Dim SelectedUnits(SelectedFighters.Count) As Unit
        Dim result As New Stack With {.leaderPos = -1}
        ReDim result.pos(UBound(busytransfer))
        Dim unitIsUsed(UBound(SelectedUnits)) As Boolean
        Dim firstRowSlots As Integer = 3
        Dim secondRowSlots As Integer = 3

        SelectedUnits(0) = Unit.Copy(AllLeaders(SelectedLeader))
        Dim n As Integer = 0
        For Each i As Integer In SelectedFighters
            n += 1
            SelectedUnits(n) = Unit.Copy(AllFighters(i))
        Next i
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
                                                          "StackStats:" & vbNewLine & DesiredStats.Print(StackStats, comm.RaceNumberToRaceChar) & vbNewLine & _
                                                          "DynStackStats:" & vbNewLine & DesiredStats.Print(DynStackStats, comm.RaceNumberToRaceChar))
        Next i
        For i As Integer = 0 To UBound(result.pos) Step 1
            If result.pos(i) = "" Then result.pos(i) = emptyitem
        Next i
        result.items = ItemsGen(DynStackStats.LootCost)
        Return result
    End Function
    Private Function SelectPossibleLeader(ByRef leaderID As Integer, ByRef Tolerance As Double, _
                                          ByRef StackStats As DesiredStats, ByRef GroundTile As Boolean) As Boolean
        If Not StackStats.Race.Contains(AllLeaders(leaderID).race) Then Return False
        If Not AllLeaders(leaderID).small And StackStats.MaxGiants = 0 Then Return False
        If AllLeaders(leaderID).waterOnly And GroundTile Then Return False
        Dim mult As Double
        If AllLeaders(leaderID).small Then
            mult = 1
        Else
            mult = 2
        End If
        If Math.Abs(AllLeaders(leaderID).EXPnext - mult * StackStats.ExpBarAverage) _
            > mult * Tolerance * StackStats.ExpBarAverage Then Return False
        If AllLeaders(leaderID).EXPkilled > (1 + Tolerance) * StackStats.ExpStackKilled Then Return False
        Return True
    End Function

    Private Function SelectFighters(ByRef skipfilter1 As Boolean, ByRef skipfilter2 As Boolean, _
                                    ByRef DynStackStats As DesiredStats, ByRef FreeMeleeSlots As Integer) As Integer

        Dim PossibleFighters As New List(Of Integer)
        Dim TExpStack As Double = DynStackStats.ExpStackKilled / DynStackStats.StackSize
        Dim SelectedFighter As Integer
        Dim nloops As Integer = 0
        Do While PossibleFighters.Count = 0 And TExpStack < 1.1 * DynStackStats.ExpStackKilled
            For j As Integer = 0 To UBound(AllFighters) Step 1
                If SelectPossibleFighter(skipfilter1, skipfilter2, j, DynStackStats, FreeMeleeSlots) Then PossibleFighters.Add(j)
            Next j
            TExpStack += 0.1 * DynStackStats.ExpStackKilled / DynStackStats.StackSize
            nloops += 1
            If nloops > 10 Then Exit Do
        Loop
        If PossibleFighters.Count > 0 Then
            SelectedFighter = comm.RandomSelection(PossibleFighters, {ExpBarFighters, ExpKilledFighters}, _
                 {DynStackStats.ExpBarAverage, CDbl(DynStackStats.ExpStackKilled) / CDbl(DynStackStats.StackSize)}, _
                 multFighters, serialExecution)
            If SelectedFighter = -1 Then Return -2
            Call ChangeLimit(AllFighters, SelectedFighter, DynStackStats, FreeMeleeSlots)
        Else
            SelectedFighter = -1
        End If
        Return SelectedFighter
    End Function
    Private Function SelectPossibleFighter(ByRef skipMaxGiantsFilter As Boolean, _
                                           ByRef skipRangeFilter As Boolean, _
                                           ByRef fighterID As Integer, _
                                           ByRef DynStackStats As DesiredStats, _
                                           ByRef FreeMeleeSlots As Integer) As Boolean
        If Not DynStackStats.Race.Contains(AllFighters(fighterID).race) Then Return False
        Dim mult As Double
        If AllFighters(fighterID).small Then
            mult = 1
        Else
            mult = 2
        End If
        If AllFighters(fighterID).EXPkilled > mult * DynStackStats.ExpStackKilled / DynStackStats.StackSize Then Return False
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

    Private Function SetUnitPosition(ByRef i As Integer, ByRef units() As Unit, _
                                     ByRef FRowSlots As Integer, ByRef SRowSlots As Integer, _
                                     ByRef AnySlot As Boolean, ByRef result As Stack) As Boolean
        Dim placed As Boolean = False
        Dim n1 As Integer = rndgen.RndPos(FRowSlots, serialExecution)
        Dim n2 As Integer = rndgen.RndPos(SRowSlots, serialExecution)

        Dim t As Integer
        Dim m As Integer = 0
        For k As Integer = 0 To UBound(firstrow) Step 1
            If Not units(i).small Or units(i).reach = 3 Then
                If result.pos(firstrow(k)) = "" Then
                    m += 1
                    If m = n1 Then
                        result.pos(firstrow(k)) = units(i).unitID
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
                            result.pos(t) = units(i).unitID
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
                        result.pos(secondrow(k)) = units(i).unitID
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

    Private Sub ChangeLimit(ByRef List() As Unit, ByRef id As Integer, _
                            ByRef DynStackStats As DesiredStats,
                            ByRef FreeMeleeSlots As Integer)
        DynStackStats.ExpStackKilled -= List(id).EXPkilled
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
            betTick = (4 + delimiterBias / 2 + Math.Pow(delimiterBias + 1, 3))
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
            RAM = (RAM + Threading.Thread.VolatileRead(tick + i)) / 2
        Next i
        'RAM = Math.Abs(RAM)
        RAM = RAM * Math.Pow(10, 15 - CInt(Math.Log10(RAM)))
        RAM = CDbl(Mid(((lastRAM + RAM) / 2).ToString, 5))
        RAM = RAM * Math.Pow(10, 15 - CInt(Math.Log10(RAM)))
        lastRAM = RAM

        tempPat += 1
        If tempPat > 20 Then tempPat = 1
        If delimiterBias > 3 Then delimiterBias = 0

        Dim c1 As Double = RAM / (tempPat + delimiterBias)
        Dim c2 As Double = tick / (100 + Math.Pow(4, 1 + delimiterBias)) * (c1 + 1) / c1
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
    Public Function Rand(ByRef lower As Double, ByRef upper As Double) As Double
        Dim r As Double = RndDbl()
        Return lower + r * (upper - lower)
    End Function
    '''<summary>Returns random value with uniform distribution. Use this to obtain more uniform distribution in the case of serial code.</summary>
    '''<param name="lower">Minimum value.</param>
    '''<param name="upper">Maximum value.</param>
    Public Function PRand(ByRef lower As Double, ByRef upper As Double) As Double
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
    Public StatFields() As StackStatsField

    Public Structure StackStatsField
        Dim name As String
        Dim description As String
    End Structure

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
        Dim k As Integer
        For i As Integer = 0 To UBound(splitedFields) Step 2
            k = CInt(i / 2)
            StatFields(k).description = splitedFields(i)
            StatFields(k).name = splitedFields(i + 1)
            StatFields(k).description = StatFields(k).description.Replace("$jm$", My.Resources.JewelItemsCostMultiplicator)
            StatFields(k).description = StatFields(k).description.Replace("$gm$", My.Resources.giantUnitsExpMultiplicator)
            StatFields(k).description = StatFields(k).description.Replace("$ri$", racesList)
            StatFields(k).description = StatFields(k).description.Replace("$newline$", vbNewLine)
        Next i
    End Sub

    ''' <summary>Читает и парсит файл с параметрами генерируемых отрядов.
    ''' Не важен порядок полей и регистр.
    ''' Не проверяет корректность данных по типу</summary>
    ''' <param name="path">Путь к файлу. Не проверяет, существует ли файл.
    ''' Если path=%testfile%, то распарсит теастовый файл.
    ''' Если path=%default%, то вернет значения, устанавливаемые для пропущенных полей.</param>
    Public Function ParseDesiredStackStatsFile(ByRef path As String) As RandStack.DesiredStats()
        Dim txt(), s(), r(), fu As String
        Dim defaultStats As New RandStack.DesiredStats With {.ExpBarAverage = 200, .ExpStackKilled = 75, _
                                                             .MeleeCount = 2, .Race = New List(Of Integer), .StackSize = 2}
        defaultStats.Race.Add(1)
        If Not path = "%testfile%" Then
            txt = TxtSplit(IO.File.ReadAllText(path).Replace("=", vbTab))
        ElseIf path = "%default%" Then
            Return New RandStack.DesiredStats() {defaultStats}
        Else
            txt = TxtSplit(My.Resources.TestStackStats.Replace("=", vbTab))
        End If
        Dim addedLabels As New List(Of String)
        Dim result(UBound(txt)) As RandStack.DesiredStats
        For i As Integer = 0 To UBound(txt) Step 1
            s = txt(i).Split(CChar(" "))
            result(i) = RandStack.DesiredStats.Copy(defaultStats)
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
                            result(i).ExpBarAverage = Math.Max(CInt(s(f + 1)), 1) 'AverageExpBar
                        ElseIf k = 2 Then
                            result(i).ExpStackKilled = Math.Max(CInt(s(f + 1)), 1) 'ExpStackKilled
                        ElseIf k = 3 Then
                            Dim rid As Integer
                            r = s(f + 1).Split(CChar("+")) 'Race
                            result(i).Race.Clear()
                            For n As Integer = 0 To UBound(r) Step 1
                                rid = RaceIdentifierToSubrace(r(n))
                                If Not result(i).Race.Contains(rid) Then result(i).Race.Add(rid)
                            Next n
                        ElseIf k = 4 Then
                            result(i).StackSize = Math.Max(CInt(s(f + 1)), 1) 'StackSize
                        ElseIf k = 5 Then
                            result(i).MaxGiants = Math.Max(CInt(s(f + 1)), 0) 'MaxGiants
                        ElseIf k = 6 Then
                            result(i).MeleeCount = Math.Max(CInt(s(f + 1)), 0) 'MeleeSlots
                        ElseIf k = 7 Then
                            result(i).LootCost = Math.Max(CInt(s(f + 1)), 0) 'LootCost
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
    Public Sub WriteDesiredStackStats(ByRef path As String, ByRef content() As RandStack.DesiredStats)
        Dim s(UBound(content)) As String
        For i As Integer = 0 To UBound(s) Step 1
            s(i) = TxtSplit(RandStack.DesiredStats.Print(content(i), RaceNumberToRaceChar).Replace(vbNewLine, vbTab))(0)
        Next i
        If Not path = "%testfile%" Then
            IO.File.WriteAllLines(path, s)
        Else
            path = ""
            For i As Integer = 0 To UBound(s) Step 1
                path &= s(i) & vbNewLine
            Next i
        End If
    End Sub

    ''' <summary>Возвращает ID, соответствующее файлам игры</summary>
    ''' <param name="ID">Идентификатор расы (файл races.txt)</param>
    Public Function RaceIdentifierToSubrace(ByRef ID As String) As Integer
        Dim uID As String = ID.ToUpper
        If Races.ContainsKey(uID) Then
            Return Races.Item(uID)
        Else
            Throw New Exception("Неизвестный идентификатор расы:" & ID)
            Return -1
        End If
    End Function
    ''' <summary>Возвращает ID, соответствующее файлам игры</summary>
    ''' <param name="ID">Идентификатор расы (файл races.txt)</param>
    Public Function RaceIdentifierToSubrace(ByRef ID As Integer) As Integer
        Return RaceIdentifierToSubrace(ID.ToString)
    End Function

    ''' <summary>Разбивает на строки текст по разделителям Chr(10) и Chr(13). Заменяет все табы на пробелы, удаляет повторяющиеся подряд пробелы, удаляет пробелы в начале и конце строки. Не добавляет в выходной массив стироки, начинающиеся с #</summary>
    ''' <param name="TXT">Какой-нибудь текст</param>
    Public Function TxtSplit(ByRef TXT As String) As String()
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
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, ByRef mult() As Double, _
                                    ByRef serial As Boolean) As Integer
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
        End If

        Do While WeightsSum = 0
            smearing += 0.1
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
            If smearing > 10 Then Return -1
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
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Stats()() As Double, _
                                    ByRef DesiredStats() As Double, _
                                    ByRef serial As Boolean) As Integer
        Return RandomSelection(IDs, Stats, DesiredStats, Nothing, serial)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка, считая, что у всех записей будет одинаковый стат. вес</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), _
                                    ByRef serial As Boolean) As Integer
        Return RandomSelection(IDs, Nothing, Nothing, serial)
    End Function
    ''' <summary>Dыбирает случайным образом запись из списка</summary>
    ''' <param name="IDs">Список намеров записей, из которых делается выбор.
    ''' Если массив Weight не инициализирован, то у всех записей будет одинаковый стат. вес</param>
    ''' <param name="Weight">Вероятность выбрать запись прямо пропорциональна величине стат. веса</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function RandomSelection(ByRef IDs As List(Of Integer), ByRef Weight() As Double, _
                                    ByRef serial As Boolean) As Integer
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
            W += tWeight(i)
            If W > R Then
                SelectedItem = i
                Exit For
            End If
        Next i
        If SelectedItem = -1 Then SelectedItem = IDs.Item(IDs.Count - 1)
        Return SelectedItem
    End Function
    Private Function Gauss(ByRef X As Double, ByRef avX As Double, ByRef sigma As Double) As Double
        Dim s As Double = 1 / (sigma * avX)
        Return Math.Exp(-0.5 * ((X - avX) * s) ^ 2) * s
    End Function

    ''' <param name="ExcludeLists">Файлы со списками исключенных объектов. Записи в них могут повторяться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default%</param>
    Public Sub ReadExcludedObjectsList(ByRef ExcludeLists() As String)
        If IsNothing(ExcludeLists) Then Exit Sub
        Dim s() As String
        For i As Integer = 0 To UBound(ExcludeLists) Step 1
            If Not ExcludeLists(i) = "%default%" Then
                If IO.File.Exists(ExcludeLists(i)) Then
                    s = TxtSplit(IO.File.ReadAllText(ExcludeLists(i)))
                Else
                    s = Nothing
                End If
            Else
                s = TxtSplit(My.Resources.ExcludeIDs)
            End If
            If Not IsNothing(s) Then
                For j As Integer = 0 To UBound(s) Step 1
                    s(j) = s(j).ToUpper
                    If Not excludedObjects.Contains(s(j)) Then excludedObjects.Add(s(j))
                Next j
            End If
        Next i
    End Sub

End Class