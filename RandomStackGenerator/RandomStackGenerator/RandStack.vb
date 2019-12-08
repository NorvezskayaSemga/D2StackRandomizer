Imports System.Threading.Tasks

Public Class RandStack



    Private busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
    Private firstrow() As Integer = New Integer() {0, 2, 4}
    Private secondrow() As Integer = New Integer() {1, 3, 5}

    Public Structure Stack
        ''' <summary>id юнита для каждой позиции</summary>
        Dim pos() As String
        ''' <summary>в какой позиции находится лидер</summary>
        Dim leaderPos As Integer
    End Structure

    Public Structure Unit
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

        Public Shared Function Copy(ByVal v As Unit) As Unit
            Return New Unit With {.level = v.level, _
                                  .race = v.race, _
                                  .EXPkilled = v.EXPkilled, _
                                  .EXPnext = v.EXPnext, _
                                  .leadership = v.leadership, _
                                  .reach = v.reach, _
                                  .unitID = v.unitID.ToUpper, _
                                  .small = v.small, _
                                  .waterOnly = v.waterOnly, _
                                  .unitBranch = v.unitBranch}
        End Function
    End Structure

    Public Structure DesiredStats
        ''' <summary>Примерная планка опыта для маленьких воинов</summary>
        Dim ExpBarAverage As Integer
        ''' <summary>Раса отряда</summary>
        Dim Race As Integer
        ''' <summary>Примерный опыт за убийство стэка</summary>
        Dim ExpStackKilled As Integer
        ''' <summary>Количество свободных ячеек под отряд. Есть 10% шанс на +1 слот и 10% неа -1 слот</summary>
        Dim StackSize As Integer
        ''' <summary>Максимальное количество маленьких воинов в отряде</summary>
        Dim MaxGiants As Integer
        ''' <summary>Сколько ячеек в первом ряду должно быть заполнено</summary>
        Dim MeleeCount As Integer

        Public Shared Function Copy(ByVal v As DesiredStats) As DesiredStats
            Return New DesiredStats With {.ExpBarAverage = v.ExpBarAverage, _
                                          .ExpStackKilled = v.ExpStackKilled, _
                                          .MaxGiants = v.MaxGiants, _
                                          .MeleeCount = v.MeleeCount, _
                                          .Race = v.Race, _
                                          .StackSize = v.StackSize}
        End Function
    End Structure

    Private AllLeaders(), AllFighters() As Unit
    Public serialExecution As Boolean
    Public rndgen As New RndValueGen

    ''' <param name="AllUnitsList">Dсе юниты в игре</param>
    ''' <param name="serial">True, если код, использующий генератор выполняется в одном потоке</param>
    Friend Sub New(ByRef AllUnitsList() As Unit, ByRef serial As Boolean)
        If IsNothing(AllUnitsList) Then Throw New Exception("Не нужно передавать неинициализированный массив")
        serialExecution = serial
        Dim splitedIDs() As String = My.Resources.ExcludeIDs.Replace(Chr(10), Chr(13)).Split(Chr(13))
        Dim exclude As New List(Of String)
        For Each item As String In splitedIDs
            If item.Length > 1 AndAlso Not item.Substring(0, 1) = "#" Then
                exclude.Add(item.ToUpper)
            End If
        Next item
        Dim nleaders, nfighters, cat(UBound(AllUnitsList)) As Integer
        nleaders = -1 : nfighters = -1
        For i As Integer = 0 To UBound(AllUnitsList) Step 1
            If Not exclude.Contains(AllUnitsList(i).unitID.ToUpper) Then
                If AllUnitsList(i).unitBranch < 5 Then
                    nfighters += 1
                    cat(i) = 1
                ElseIf AllUnitsList(i).unitBranch = 5 Then
                    nleaders += 1
                    cat(i) = 2
                End If
            End If
        Next i
        ReDim AllLeaders(nleaders), AllFighters(nfighters)
        nleaders = -1 : nfighters = -1
        For i As Integer = 0 To UBound(AllUnitsList) Step 1
            If cat(i) = 1 Then
                nfighters += 1
                AllFighters(nfighters) = Unit.Copy(AllUnitsList(i))
                AllFighters(nfighters).race = SubraceToRace(AllFighters(nfighters).race)
            ElseIf cat(i) = 2 Then
                nleaders += 1
                AllLeaders(nleaders) = Unit.Copy(AllUnitsList(i))
                AllLeaders(nleaders).race = SubraceToRace(AllLeaders(nleaders).race)
            End If
        Next i
    End Sub
    Private Function SubraceToRace(ByRef subrace As Integer) As Integer
        If subrace = 6 Then
            Return 1
        ElseIf subrace = 7 Then
            Return 14
        Else
            Return subrace
        End If
    End Function

    ''' <summary>Затычка: вернет отряд из двух сквайров и трех лучников. Лидер - паладин</summary>
    Friend Function GenGag() As Stack
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
        Return result
    End Function

    ''' <param name="StackStats">Желаемые параметры стэка</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров</param>
    Friend Function Gen(ByRef StackStats As DesiredStats, ByRef GroundTile As Boolean) As Stack

        Dim PossibleLeaders, SelectedFighters As New List(Of Integer)

        'создаем список лидеров, которых вообще можем использовать
        PossibleLeaders.Clear()
        Dim Tolerance As Double = 0
        Do While PossibleLeaders.Count = 0
            Tolerance += 0.1
            For i As Integer = 0 To UBound(AllLeaders) Step 1
                If SelectPossibleLeader(i, Tolerance, StackStats, GroundTile) Then PossibleLeaders.Add(i)
            Next i
        Loop

        Dim SelectedLeader As Integer = RandomSelection(AllLeaders, PossibleLeaders, 0, StackStats)
        Dim FreeMeleeSlots As Integer = 3

        'теперь нужно добрать воинов в отряд
        Dim DynStackStats As DesiredStats = DesiredStats.Copy(StackStats)
        Dim R As Double = Rand(0, 1, serialExecution)
        If R < 0.1 Then
            DynStackStats.StackSize -= 1
        ElseIf R > 0.9 Then
            DynStackStats.StackSize += 1
        End If
        DynStackStats.StackSize = Math.Min(DynStackStats.StackSize, AllLeaders(SelectedLeader).leadership) - 1
        If AllLeaders(SelectedLeader).small Then
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 1)
        Else
            DynStackStats.StackSize = Math.Max(DynStackStats.StackSize, 2)
        End If

        Call ChangeLimit(AllLeaders, SelectedLeader, DynStackStats, FreeMeleeSlots)

        Dim fighter As Integer
        For i As Integer = 1 To DynStackStats.StackSize Step 1
            'создаем список воинов, которых можно использовать
            fighter = SelectFighters(False, False, i, DynStackStats, FreeMeleeSlots)
            If fighter = -1 Then
                fighter = SelectFighters(True, False, i, DynStackStats, FreeMeleeSlots)
                If fighter = -1 Then fighter = SelectFighters(True, True, i, DynStackStats, FreeMeleeSlots)
            End If
            If fighter = -1 Then
                Exit For
            Else
                SelectedFighters.Add(fighter)
            End If
        Next i
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
                Call SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
                unitIsUsed(i) = True
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And SelectedUnits(i).reach = 3 Then
                Call SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
                unitIsUsed(i) = True
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) And Not SelectedUnits(i).reach = 3 Then
                Call SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, False, result)
                unitIsUsed(i) = True
            End If
        Next i
        For i As Integer = 0 To UBound(SelectedUnits) Step 1
            If Not unitIsUsed(i) Then
                Call SetUnitPosition(i, SelectedUnits, firstRowSlots, secondRowSlots, True, result)
                unitIsUsed(i) = True
            End If
        Next i
        For i As Integer = 0 To UBound(result.pos) Step 1
            If result.pos(i) = "" Then result.pos(i) = "G000000000"
        Next i
        Return result
    End Function
    Private Function SelectPossibleLeader(ByRef leaderID As Integer, ByRef Tolerance As Double, _
                                          ByRef StackStats As DesiredStats, ByRef GroundTile As Boolean) As Boolean
        If Not AllLeaders(leaderID).race = StackStats.Race Then Return False
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

    Private Function SelectFighters(ByRef skipfilter1 As Boolean, ByRef skipfilter2 As Boolean, ByRef i As Integer, _
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
            SelectedFighter = RandomSelection(AllFighters, PossibleFighters, 1, DynStackStats)
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
        If Not AllFighters(fighterID).race = DynStackStats.Race Then Return False
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

    Private Sub SetUnitPosition(ByRef i As Integer, ByRef units() As Unit, _
                                ByRef FRowSlots As Integer, ByRef SRowSlots As Integer, _
                                ByRef AnySlot As Boolean, ByRef result As Stack)
        Dim placed As Boolean = False
        Dim n1 As Integer = RndPos(FRowSlots, serialExecution)
        Dim n2 As Integer = RndPos(SRowSlots, serialExecution)

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
                            result.pos(busytransfer(firstrow(k))) = "G000000000"
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
                                    Exit For
                                    placed = True
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
        If Not placed Then Throw New Exception("Что-то не так в размещателе юнитов")
    End Sub

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
            FreeMeleeSlots -= 1
        End If
    End Sub

    'выбираем случайным образом запись из списка
    Private Function RandomSelection(ByRef List() As Unit, ByRef IDs As List(Of Integer), ByRef mode As Integer, _
                                     ByRef DynStackStats As DesiredStats) As Integer
        Dim WeightsSum As Double = 0
        Dim Weight(UBound(List)) As Double
        Dim smearing As Double = 0
        Dim mult As Double

        Do While WeightsSum = 0
            smearing += 0.1
            For Each i As Integer In IDs
                If List(i).small Then
                    mult = 1
                Else
                    mult = 2
                End If
                Weight(i) = Gauss(CDbl(List(i).EXPnext), mult * CDbl(DynStackStats.ExpBarAverage), smearing)
                If mode = 1 Then Weight(i) *= Gauss(CDbl(List(i).EXPkilled), mult * CDbl(DynStackStats.ExpStackKilled) / CDbl(DynStackStats.StackSize), smearing)
                WeightsSum += Weight(i)
            Next i
            If smearing = 10 Then Throw New Exception("Возможно, бесконечный цикл в случайном выборе из массива")
        Loop

        Dim R As Double = Rand(0, WeightsSum, serialExecution)
        Dim W As Double = 0
        Dim SelectedItem As Integer = -1
        For Each i In IDs
            W += Weight(i)
            If W > R Then
                SelectedItem = i
                Exit For
            End If
        Next i
        If SelectedItem = -1 Then SelectedItem = IDs.Item(IDs.Count - 1)
        Return SelectedItem
    End Function
    Private Function Gauss(ByRef X As Double, ByRef avX As Double, ByRef sigma As Double) As Double
        Return Math.Exp(-0.5 * ((X - avX) / (sigma * avX)) ^ 2)
    End Function

    '''<summary>Returns random value with uniform distribution.</summary>
    '''<param name="lower">Minimum value.</param>
    '''<param name="upper">Maximum value.</param>
    ''' <param name="serial">True, if use in serial code</param>
    Private Function Rand(ByRef lower As Double, ByRef upper As Double, ByRef serial As Boolean) As Double
        If serial Then
            Return rndgen.PRand(lower, upper)
        Else
            Return rndgen.Rand(lower, upper)
        End If
    End Function
    Private Function RndPos(ByRef n As Integer, ByRef serial As Boolean) As Integer
        Dim R As Double
        If serial Then
            R = rndgen.PRand(0, 1)
        Else
            R = rndgen.Rand(0, 1)
        End If
        Dim dr As Double = 1 / n
        For i As Integer = 1 To n Step 1
            If CDbl(i) * dr >= R Then Return i
        Next i
        Return n
    End Function

End Class

Public Class RndValueGen
    Private betTick, lastRAM As Double
    Private tempPat, delimiterBias As Integer

    Friend Sub New()
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

End Class
