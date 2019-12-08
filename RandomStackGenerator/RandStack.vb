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
        ''' <summary>True, если может быть лидером отряда</summary>
        Dim isLeader As Boolean
    End Structure

    Dim AllLeaders(), AllFighters() As Unit
    Public serialExecution As Boolean
    Public rndgen As New RndValueGen

    ''' <param name="leadersList">Все лидеры за исключением саммонов и лидеров отрядов играбельных рас</param>
    ''' <param name="fightersList">Dсе юнитs нелидерs</param>
    ''' <param name="serial">True, если код, использующий генератор выполняется в одном потоке</param>
    Friend Sub New(ByRef leadersList() As Unit, ByRef fightersList() As Unit, ByRef serial As Boolean)
        If IsNothing(AllFighters) Or IsNothing(AllLeaders) Then Throw New Exception("Не нужно передавать неинициализированные массивы")
        serialExecution = serial
    End Sub

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

    ''' <param name="DesiredExp">Желательная планка опыта юнитов в отряде</param>
    ''' <param name="DesiredRace">Раса юнитов</param>
    ''' <param name="DesiredExpStack">Желательный опыт, получаемый за отряд</param>
    ''' <param name="DesiredStackSize">Количество слотов в отряде</param>
    ''' <param name="DesiredMaxGiants">Максимальное количество больших юнитов в отряде</param>
    ''' <param name="DesiredMelee">Количество юнитов ближнего боя в отряде</param>
    ''' <param name="GroundTile">True, если на клетку нельзя ставить водных лидеров</param>
    Friend Function Gen(ByRef DesiredExp As Integer, ByRef DesiredRace As Integer, ByRef DesiredExpStack As Integer, _
                        ByRef DesiredStackSize As Integer, ByRef DesiredMaxGiants As Integer, ByRef DesiredMelee As Integer, _
                        ByRef GroundTile As Boolean) As Stack

        Dim PossibleLeaders, SelectedFighters As New List(Of Integer)

        'создаем список лидеров, которых вообще можем использовать
        PossibleLeaders.Clear()
        Dim Tolerance As Double = 0
        Do While PossibleLeaders.Count = 0
            Tolerance += 0.1
            For i As Integer = 0 To UBound(AllLeaders) Step 1
                If AllLeaders(i).race = DesiredRace And Math.Abs(AllLeaders(i).EXPnext - DesiredExp) < Tolerance * DesiredExp Then
                    If AllLeaders(i).small Or (Not AllLeaders(i).small And DesiredMaxGiants > 0) Then
                        If Not AllLeaders(i).waterOnly Or (AllLeaders(i).waterOnly And Not GroundTile) Then
                            PossibleLeaders.Add(i)
                        End If
                    End If
                End If
            Next i
        Loop

        Dim SelectedLeader As Integer = RandomSelection(AllLeaders, PossibleLeaders, 0, DesiredExp, 0)

        'теперь нужно добрать воинов в отряд
        Dim MaxSlots As Integer = DesiredStackSize
        Dim R As Double = rndgen.Rand(0, 1, serialExecution)
        If R < 0.1 Then
            MaxSlots -= 1
        ElseIf R > 0.9 Then
            MaxSlots -= 1
        End If
        MaxSlots = Math.Min(MaxSlots, AllLeaders(SelectedLeader).leadership)

        Dim ExpStack As Integer = DesiredExpStack
        Dim MaxGiants As Integer = DesiredMaxGiants
        Dim MinMelee As Integer = DesiredMelee

        Call ChangeLimit(AllLeaders, SelectedLeader, ExpStack, MaxGiants, MinMelee, MaxSlots)

        Dim fighter As Integer
        For i As Integer = 2 To MaxSlots Step 1
            'создаем список воинов, которых можно использовать
            fighter = func_SelectFighters(False, False, i, ExpStack, MaxGiants, MinMelee, MaxSlots, DesiredRace, DesiredExp)
            If fighter = -1 Then
                fighter = func_SelectFighters(True, False, i, ExpStack, MaxGiants, MinMelee, MaxSlots, DesiredRace, DesiredExp)
                If fighter = -1 Then fighter = func_SelectFighters(True, True, i, ExpStack, MaxGiants, MinMelee, MaxSlots, DesiredRace, DesiredExp)
            End If
            If fighter = -1 Then
                Exit For
            Else
                SelectedFighters.Add(fighter)
            End If
        Next i
        'в итоге должны получить лидера и остальной отряд
        'дальше расставляем в зависимости от размера и дальности атаки и пишем в файл карты

        Dim unitID(SelectedFighters.Count) As String
        Dim melee(UBound(unitID)), giant(UBound(unitID)) As Boolean
        Dim result As Stack
        ReDim result.pos(UBound(busytransfer))
        Dim busy(UBound(result.pos)) As Boolean

        unitID(0) = AllLeaders(SelectedLeader).unitID
        melee(0) = (AllLeaders(SelectedLeader).reach = 3)
        giant(0) = Not AllLeaders(SelectedLeader).small
        Dim n As Integer = 0
        Dim m As Integer
        Dim FRowSlots As Integer = 3
        Dim SRowSlots As Integer = 3

        For Each i As Integer In SelectedFighters
            n += 1
            unitID(n) = AllFighters(i).unitID
            melee(n) = (AllFighters(i).reach = 3)
            giant(n) = Not AllFighters(i).small
        Next i
        For i As Integer = 0 To UBound(unitID) Step 1
            If giant(i) Then
            End If
        Next i
        Return result
    End Function
    Private Sub SetUnitPosition(ByRef i As Integer, _
                                ByRef unitID() As String, ByRef giant() As Boolean, ByRef melee() As Boolean, _
                                ByRef FRowSlots As Integer, ByRef SRowSlots As Integer, ByRef busy() As Boolean, _
                                ByRef result As Stack)
        Dim n As Integer = rndgen.RndPos(FRowSlots, serialExecution)
        Dim m As Integer = 0
        For k As Integer = 0 To UBound(firstrow) Step 1
            If Not busy(firstrow(k)) Then
                m += 1
                If m = n Then
                    busy(firstrow(k)) = True
                    result.pos(firstrow(k)) = unitID(i)
                    If giant(i) Then result.pos(busytransfer(firstrow(k))) = "G000000000"
                    If i = 0 Then result.leaderPos = firstrow(k)

                    Exit For
                End If
            End If
        Next k

    End Sub
    Private Sub ChangeLimit(ByRef List() As Unit, ByRef id As Integer, _
                            ByRef ExpStack As Integer, ByRef MaxGiants As Integer, _
                            ByRef MinMelee As Integer, ByRef MaxSlots As Integer)
        ExpStack -= List(id).EXPkilled
        If Not List(id).small Then
            MaxGiants -= 1
            MaxSlots -= 1
            MinMelee = Math.Max(0, MinMelee - 1)
        ElseIf List(id).reach = 3 Then
            MinMelee = Math.Max(0, MinMelee - 1)
        End If
    End Sub

    'выбираем случайным образом запись из списка
    Private Function RandomSelection(ByRef List() As Unit, ByRef IDs As List(Of Integer), ByRef mode As Integer, _
                                     ByRef DesiredExp As Integer, ByRef DesiredExpKill As Integer) As Integer
        Dim WeightsSum As Double = 0
        Dim Weight(UBound(List)) As Double
        Dim smearing As Double = 0

        Do While WeightsSum = 0
            WeightsSum = 0
            smearing += 0.1
            For Each i As Integer In IDs
                Weight(i) = func_Gauss(CDbl(List(i).EXPnext), CDbl(DesiredExp), smearing)
                If mode = 1 Then Weight(i) *= func_Gauss(CDbl(List(i).EXPkilled), CDbl(DesiredExpKill), smearing)
                WeightsSum += Weight(i)
            Next i
            If smearing = 10 Then Throw New Exception("Возможно, бесконечный цикл в случайном выборе из массива")
        Loop

        Dim R As Double = rndgen.Rand(0, WeightsSum, serialExecution)
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

    Private Function func_Gauss(ByRef X As Double, ByRef avX As Double, ByRef sigma As Double) As Double
        Return Math.Exp(-0.5 * ((X - avX) / (sigma * avX)) ^ 2)
    End Function


    Private Function func_SelectFighters(ByRef skipfilter1 As Boolean, ByRef skipfilter2 As Boolean, ByRef i As Integer, _
                                         ByRef ExpStack As Integer, ByRef MaxGiants As Integer, ByRef MinMelee As Integer, _
                                         ByRef MaxSlots As Integer, ByRef DesiredRace As Integer, ByRef DesiredExp As Integer) As Integer
        '(остальные переменные те же, что и в main; ExpStack, MaxSlots, MaxGiants и MinMelee обязательно передавать by reference)
        Dim AverageExpStack As Double = ExpStack / (MaxSlots - i + 1)
        Dim PossibleFighters As New List(Of Integer)
        Dim TExpStack As Double = AverageExpStack
        Dim SelectedFighter As Integer
        Dim nloops As Integer = 0
        Do While PossibleFighters.Count = 0 And TExpStack < 1.1 * ExpStack
            For j As Integer = 0 To UBound(AllFighters) Step 1
                If AllFighters(j).race = DesiredRace And AllFighters(j).EXPkilled < TExpStack Then
                    If (AllFighters(j).small Or (Not AllFighters(j).small And MaxGiants > 0 And MaxSlots - i > 0)) Or skipfilter1 Then
                        If ((AllFighters(j).reach = 3 And MinMelee > 0) Or (Not AllFighters(j).reach = 3 And MinMelee = 0)) Or skipfilter2 Then
                            PossibleFighters.Add(j)
                        End If
                    End If
                End If
            Next j
            TExpStack += 0.1 * AverageExpStack
            nloops += 1
            If nloops > 10 Then Exit Do
        Loop
        If PossibleFighters.Count > 0 Then
            SelectedFighter = RandomSelection(AllFighters, PossibleFighters, 1, DesiredExp, CInt(AverageExpStack))
            Call ChangeLimit(AllLeaders, SelectedFighter, ExpStack, MaxGiants, MinMelee, MaxSlots)
        Else
            SelectedFighter = -1
        End If
        Return SelectedFighter
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

    '''<summary>Returns random value with uniform distribution.</summary>
    '''<param name="lower">Minimum value.</param>
    '''<param name="upper">Maximum value.</param>
    ''' <param name="serial">True, if use in serial code</param>
    Public Function Rand(ByRef lower As Double, ByRef upper As Double, ByRef serial As Boolean) As Double
        If serial Then
            Return PRand(lower, upper)
        Else
            Return Rand(lower, upper)
        End If
    End Function
    Public Function RndPos(ByRef n As Integer, ByRef serial As Boolean) As Integer
        Dim R As Double
        If serial Then
            R = PRand(0, 1)
        Else
            R = Rand(0, 1)
        End If
        Dim dr As Double = 1 / n
        For i As Integer = 1 To n Step 1
            If CDbl(i) * dr >= R Then Return i
        Next i
        Return n
    End Function

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
