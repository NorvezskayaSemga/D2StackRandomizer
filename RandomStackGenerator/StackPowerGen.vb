Public Class StackPowerGen

    Private rndgen As New RndValueGen
    Private imp As ImpenetrableMeshGen
    Private defValues As GenDefaultValues

    Private Structure StackLoc
        Dim pos As Point
        Dim CenDist As Double
        Dim CapDist As Double
        Dim isPassGuard As Boolean
        Dim isObjectGuard As Boolean
        Dim LocID As Integer
    End Structure

    '''<summary>Заполнит m.groupStats, где ключ - ID группы, значение - параметры генерации без определенной расы отряда</summary>
    ''' <param name="m">Заготовка карты после работы генератора положения отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества опыта для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2)</param>>
    Public Sub Gen(ByRef m As Map, _
                   ByRef settMap As Map.SettingsMap, _
                   ByRef settRaceLoc As Map.SettingsLoc, _
                   ByRef settCommLoc As Map.SettingsLoc)
        'If Not settRaceLoc.isChecked Then Throw New Exception("Check parameters via settRaceLoc.Check()")
        'If Not settCommLoc.isChecked Then Throw New Exception("Check parameters via settCommLoc.Check()")
        Dim a() As Map.SettingsLoc = Map.SettingsLoc.ToArray(settRaceLoc, settCommLoc, settMap.nRaces, m.Loc.Length)
        Call Gen(m, settMap, a)
    End Sub
    '''<summary>Заполнит m.groupStats, где ключ - ID группы, значение - параметры генерации без определенной расы отряда</summary>
    ''' <param name="m">Заготовка карты после работы генератора положения отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settLoc">Настройки для каждой локации. Первыми должны идти стартовые локации рас.
    ''' Коментарий к настройкам стартовых локаций играбельных рас:
    ''' дробная часть определяет шанс округления большую сторону.
    ''' Комментарий к настройкам остальных локаций:
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади, заданной в настройках (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт </param>
    Public Sub Gen(ByRef m As Map, _
                   ByRef settMap As Map.SettingsMap, _
                   ByRef settLoc() As Map.SettingsLoc)

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        'For i As Integer = 0 To UBound(settLoc) Step 1
        '    If Not settLoc(i).isChecked Then Throw New Exception("Check parameters via settLoc(" & i & ").Check()")
        'Next i

        If Not m.complited.StacksPlacing_Done Or Not m.complited.MeshTestII_Done Then
            Throw New Exception("Сначала нужно выполнить StackLocations.Gen " & _
                                "и протестировать результат с помощью m.TestMap")
        End If

        defValues = m.comm.defValues
        imp = New ImpenetrableMeshGen(m.comm)

        Dim t0 As Integer = Environment.TickCount
        Dim guards As Dictionary(Of Integer, StackLoc) = MakeGuardsList(m, settMap)
        Dim LocTotalExp() As Double = MakeLocationsList(m, settMap, settLoc)

        m.groupStats = GenStacksStats(m, settMap, settLoc, guards, LocTotalExp)

        m.complited.StacksDesiredStatsGen_Done = True

        Call m.log.Add("Stacks stats generation: " & Environment.TickCount - t0 & " ms")
    End Sub

    Private Function MakeGuardsList(ByRef m As Map, ByRef settMap As Map.SettingsMap) As Dictionary(Of Integer, StackLoc)
        Dim locs As New Dictionary(Of Integer, StackLoc)
        Dim CapPos As New List(Of Point)
        Dim centerX, centerY As Double
        centerX = 0.5 * m.xSize
        centerY = 0.5 * m.ySize
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).passability.isAttended AndAlso m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                    CapPos.Add(New Point(CInt(x + 0.5 * (imp.ActiveObjects(m.board(x, y).mapObject.objectID).Size - 1)), _
                                         CInt(y + 0.5 * (imp.ActiveObjects(m.board(x, y).mapObject.objectID).Size - 1))))
                End If
            Next x
        Next y
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If (m.board(x, y).stack.GuardLoc Or m.board(x, y).stack.PassGuardLoc) AndAlso Not locs.ContainsKey(m.board(x, y).groupID) Then
                    Dim CenDist As Double = New Point(x, y).Dist(centerX, centerY)
                    Dim CapDist As Double = Double.MaxValue
                    For Each p As Point In CapPos
                        CapDist = Math.Min(CapDist, p.Dist(x, y))
                    Next p
                    Dim g As Integer = m.board(x, y).groupID
                    locs.Add(g, New StackLoc With {.CapDist = CapDist, _
                                                   .CenDist = CenDist, _
                                                   .isPassGuard = m.board(x, y).stack.PassGuardLoc, _
                                                   .isObjectGuard = m.board(x, y).stack.ObjectGuard, _
                                                   .pos = New Point(x, y), _
                                                   .LocID = m.board(x, y).locID(0)})
                    If m.board(x, y).mapObject.objectID = DefMapObjects.Types.City And m.board(x, y).locID(0) > settMap.nRaces Then
                        If g = 0 Then Throw New Exception("Unexpected group ID of city guards")
                        locs.Add(-g, locs.Item(g)) 'm.groupStats.Add(-
                    End If
                End If
            Next x
        Next y
        Return locs
    End Function
    Private Function MakeLocationsList(ByRef m As Map, _
                                       ByRef settMap As Map.SettingsMap, _
                                       ByRef settLoc() As Map.SettingsLoc) As Double()

        Dim CapPos As New List(Of Point)
        Dim centerX, centerY As Double
        centerX = 0.5 * m.xSize
        centerY = 0.5 * m.ySize
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).passability.isAttended AndAlso m.board(x, y).mapObject.objectID = 1 Then
                    CapPos.Add(New Point(CInt(x + 0.5 * (imp.ActiveObjects(m.board(x, y).mapObject.objectID).Size - 1)), _
                                         CInt(y + 0.5 * (imp.ActiveObjects(m.board(x, y).mapObject.objectID).Size - 1))))
                End If
            Next x
        Next y

        Dim Area(UBound(m.Loc)) As Integer
        Dim Lpos(UBound(m.Loc)) As Point
        Dim LExp(UBound(m.Loc)), LWeight(UBound(m.Loc)), Wmin, Wmax, _
            CapDist(UBound(m.Loc)), CenDist(UBound(m.Loc)), _
            minCapDist, maxCapDist, minCenDist, maxCenDist As Double
        For i As Integer = 0 To UBound(m.Loc) Step 1
            Lpos(i) = New Point(0, 0)
        Next i
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not m.board(x, y).passability.isBorder And Not m.board(x, y).passability.isAttended Then
                    Dim id As Integer = m.board(x, y).locID(0) - 1
                    Area(id) += 1
                    Lpos(id).X += x
                    Lpos(id).Y += y
                End If
            Next x
        Next y
        For i As Integer = 0 To UBound(m.Loc) Step 1
            Lpos(i).X = CInt(Lpos(i).X / Area(i))
            Lpos(i).Y = CInt(Lpos(i).Y / Area(i))
            LExp(i) = settLoc(i).expAmount * settMap.StackStrength
            If settLoc(i).scaleContent Then LExp(i) *= Area(i) / (Math.PI * Math.Pow(settLoc(i).AverageRadius - 1, 2))
            'If i >= CapPos.Count Then LExp(i) *= Area(i) / (Math.PI * Math.Pow(settLoc(i).AverageRadius - 1, 2))
            'If i < CapPos.Count Then
            '    LExp(i) = settRaceLoc.expAmount
            'Else
            '    LExp(i) = settCommLoc.expAmount * Area(i) / (Math.PI * Math.Pow(settCommLoc.AverageRadius - 1, 2))
            'End If
        Next i
        minCapDist = Double.MaxValue : maxCapDist = Double.MinValue
        minCenDist = Double.MaxValue : maxCenDist = Double.MinValue
        Wmin = Double.MaxValue : Wmax = Double.MinValue
        For i As Integer = CapPos.Count To UBound(m.Loc) Step 1
            CenDist(i) = Lpos(i).Dist(centerX, centerY)
            CapDist(i) = Double.MaxValue
            For Each p As Point In CapPos
                CapDist(i) = Math.Min(CapDist(i), p.Dist(Lpos(i)))
            Next p
            minCapDist = Math.Min(minCapDist, CapDist(i))
            maxCapDist = Math.Max(maxCapDist, CapDist(i))
            minCenDist = Math.Min(minCenDist, CenDist(i))
            maxCenDist = Math.Max(maxCenDist, CenDist(i))
        Next i
        For i As Integer = CapPos.Count To UBound(m.Loc) Step 1
            Dim w1 As Double = CapitalDistWeight(CapDist(i), minCapDist, maxCapDist)
            Dim w2 As Double = CenterDistWeight(CenDist(i), minCenDist, maxCenDist)
            LWeight(i) = w1 + w2
            Wmin = Math.Min(Wmin, LWeight(i))
            Wmax = Math.Max(Wmax, LWeight(i))
        Next i
        Dim m1, m2 As Double
        If Wmax > Wmin Then
            m1 = Common.ValueLowerBound(settMap.LocExpRatio, 1)
            m2 = (Common.ValueUpperBound(settMap.LocExpRatio, 1) - m1) / (Wmax - Wmin)
        Else
            m1 = 1
            m2 = 0
        End If
        For i As Integer = CapPos.Count To UBound(m.Loc) Step 1
            LExp(i) *= m1 + m2 * (LWeight(i) - Wmin)
        Next i
        Return LExp
    End Function
    Private Function CapitalDistWeight(ByRef dist As Double, ByRef minDist As Double, ByRef maxDist As Double) As Double
        Dim w As Double = 0.5 + (dist - minDist) / (maxDist - minDist + 0.000001)
        'Console.WriteLine("Cap W " & w)
        Return w
    End Function
    Private Function CenterDistWeight(ByRef dist As Double, ByRef minDist As Double, ByRef maxDist As Double) As Double
        Dim w As Double = 0.5 + (maxDist - dist) / (maxDist - minDist + 0.000001)
        'Console.WriteLine("Cen W " & w)
        Return w
    End Function

    Private Function GenStacksStats(ByRef m As Map, _
                                    ByRef settMap As Map.SettingsMap, _
                                    ByRef settLoc() As Map.SettingsLoc, _
                                    ByRef guards As Dictionary(Of Integer, StackLoc), _
                                    ByRef LocTotalExp() As Double) As Dictionary(Of Integer, AllDataStructues.DesiredStats)

        Dim res As New Dictionary(Of Integer, AllDataStructues.DesiredStats)
        Dim W, WLoot As New Dictionary(Of Integer, Double)
        Dim Wsum(UBound(LocTotalExp)), WLootSum(UBound(LocTotalExp)) As Double
        Dim minCapDist, maxCapDist, minCenDist, maxCenDist As Double
        For i As Integer = 0 To UBound(m.Loc) Step 1
            minCapDist = Double.MaxValue : maxCapDist = Double.MinValue
            minCenDist = Double.MaxValue : maxCenDist = Double.MinValue
            For Each id As Integer In guards.Keys
                If i = guards.Item(id).LocID - 1 And Not guards.Item(id).isPassGuard Then
                    Dim ca As Double = guards.Item(id).CapDist
                    Dim ce As Double = guards.Item(id).CenDist
                    minCapDist = Math.Min(minCapDist, ca)
                    maxCapDist = Math.Max(maxCapDist, ca)
                    minCenDist = Math.Min(minCenDist, ce)
                    maxCenDist = Math.Max(maxCenDist, ce)
                End If
            Next id

            For Each id As Integer In guards.Keys
                If i = guards.Item(id).LocID - 1 Then
                    If Not guards.Item(id).isPassGuard Then
                        Dim x As Integer = guards.Item(id).pos.X
                        Dim y As Integer = guards.Item(id).pos.Y
                        Dim r As Double = rndgen.PRand(0.85, 1.15)
                        Dim t As Double = CapitalDistWeight(guards.Item(id).CapDist, minCapDist, maxCapDist)
                        If i >= settMap.nRaces Then t += CenterDistWeight(guards.Item(id).CenDist, minCenDist, maxCenDist)
                        t *= r
                        If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins Then
                            t *= settLoc(m.board(x, y).locID(0) - 1).ruinsPowerMultiplicator
                        ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                            t *= settLoc(m.board(x, y).locID(0) - 1).citiesPowerMultiplicator
                        End If
                        W.Add(id, t)
                        Wsum(guards.Item(id).LocID - 1) += t
                    Else
                        W.Add(id, 0)
                    End If
                End If
            Next id
        Next i

        Dim minD As Double = Common.ValueLowerBound(defValues.LootCostDispersion, settMap.Wealth)
        Dim maxD As Double = Common.ValueUpperBound(defValues.LootCostDispersion, settMap.Wealth)

        Dim expKilled, LootCost As New Dictionary(Of Integer, Double)
        For Each id As Integer In guards.Keys
            If Not guards.Item(id).isPassGuard Then
                Dim x As Integer = guards.Item(id).pos.X
                Dim y As Integer = guards.Item(id).pos.Y
                Dim stackPowerMultiplier As Double
                If guards.Item(id).isObjectGuard Then
                    stackPowerMultiplier = settMap.ObjectGuardsPowerMultiplicator
                Else
                    stackPowerMultiplier = 1
                End If
                Dim e As Double = stackPowerMultiplier * LocTotalExp(guards.Item(id).LocID - 1) * W.Item(id) / Wsum(guards.Item(id).LocID - 1)
                expKilled.Add(id, e)
                Dim t As Double = e * rndgen.PRand(minD, maxD)
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins Then
                    t *= settLoc(m.board(x, y).locID(0) - 1).ruinsWealthMultiplicator
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                    t *= settLoc(m.board(x, y).locID(0) - 1).citiesWealthMultiplicator
                End If
                WLoot.Add(id, t)
                WLootSum(guards.Item(id).LocID - 1) += t
            End If
        Next id
        For Each id As Integer In guards.Keys
            If Not guards.Item(id).isPassGuard Then
                If WLootSum(guards.Item(id).LocID - 1) > 0 Then
                    LootCost.Add(id, settMap.Wealth * WLoot.Item(id) * LocTotalExp(guards.Item(id).LocID - 1) / WLootSum(guards.Item(id).LocID - 1))
                Else
                    LootCost.Add(id, 0)
                End If
            End If
        Next id

        For Each id As Integer In guards.Keys
            If guards.Item(id).isPassGuard Then
                Dim n As Integer = 10
                Dim nearest As New List(Of Integer)
                Dim expSum As Double
                Dim e As Double
                Do While n < 205
                    nearest.Clear()
                    For Each k As Integer In guards.Keys
                        If Not guards.Item(k).isPassGuard Then
                            Dim d As Integer = guards.Item(id).pos.SqDist(guards.Item(k).pos)
                            If d <= n Then nearest.Add(k)
                        End If
                    Next k
                    If nearest.Count > 4 Or (n + 5 >= 205 And nearest.Count > 0) Then
                        Exit Do
                    Else
                        n += 5
                    End If
                Loop
                If n < 205 Then
                    expSum = 0
                    For Each k As Integer In nearest
                        expSum += expKilled.Item(k)
                    Next k
                    e = expSum / nearest.Count
                Else
                    e = rndgen.RndInt(200, 300, True)
                End If
                expKilled.Add(id, e * settMap.PassGuardsPowerMultiplicator)
                LootCost.Add(id, 0)
            End If
        Next id
        For Each id As Integer In guards.Keys
            res.Add(id, GenDesiredStats(expKilled.Item(id), LootCost.Item(id), id))
        Next id
        Return res
    End Function
    Private Function GenDesiredStats(ByRef expKilled As Double, ByRef LootCost As Double, ByRef groupID As Integer) As AllDataStructues.DesiredStats
        Return StackStatsGen.GenDesiredStats(expKilled, LootCost, rndgen, defValues, groupID)
    End Function
End Class

Class StackStatsGen

    Private Shared Function UnitExpKilledToExpBar(ByRef value As Double) As Double
        Return Math.Max(8.4639 * value - 72.748, 50)
    End Function
    Private Shared Function UnitExpKilledToExpBar(ByRef value As Integer) As Integer
        Return CInt(UnitExpKilledToExpBar(CDbl(value)))
    End Function
    Private Shared Function UnitExpBarToExpKilled(ByRef value As Double) As Double
        Return Math.Max(0.1178 * value + 8.8477, 10)
    End Function
    Private Shared Function UnitExpBarToExpKilled(ByRef value As Integer) As Integer
        Return CInt(UnitExpBarToExpKilled(CDbl(value)))
    End Function

    Friend Shared Function GenDesiredStats(ByRef expKilled As Double, ByRef LootCost As Double, ByRef rndGen As RandomStackGenerator.RndValueGen, ByRef defValues As GenDefaultValues) As AllDataStructues.DesiredStats
        Return StackStatsGen.GenDesiredStats(expKilled, LootCost, rndGen, defValues, -1)
    End Function
    Friend Shared Function GenDesiredStats(ByRef expKilled As Double, ByRef LootCost As Double, ByRef rndGen As RandomStackGenerator.RndValueGen, ByRef defValues As GenDefaultValues, ByRef groupID As Integer) As AllDataStructues.DesiredStats
        Dim stackSize As Integer = rndGen.RndPos(defValues.maxStackSize, True)
        Dim meleeCount As Integer = rndGen.RndPos(Math.Min(stackSize, 3), True)
        Dim maxGiants As Integer
        If stackSize < 2 Then
            maxGiants = 0
        ElseIf stackSize < defValues.maxStackSize - 2 Then
            maxGiants = rndGen.RndPos(2, True) - 1
        ElseIf stackSize < defValues.maxStackSize Then
            maxGiants = rndGen.RndPos(3, True) - 1
        Else
            maxGiants = rndGen.RndPos(4, True) - 1
        End If

        Dim averageExpBar As Double = UnitExpKilledToExpBar(expKilled / (stackSize - 0.5 * maxGiants))
        Dim minExpBar As Double = Common.ValueLowerBound(defValues.expBarDispersion, averageExpBar)
        Dim maxExpBar As Double = Common.ValueUpperBound(defValues.expBarDispersion, averageExpBar)
        Dim eBar As Integer = CInt(rndGen.PRand(minExpBar, maxExpBar))

        Return New AllDataStructues.DesiredStats With {.ExpStackKilled = Math.Max(CInt(expKilled), 5), _
                                                       .StackSize = stackSize, _
                                                       .MeleeCount = meleeCount, _
                                                       .MaxGiants = maxGiants, _
                                                       .Race = New List(Of Integer), _
                                                       .LootCost = CInt(LootCost), _
                                                       .LocationName = "Loc_" & groupID, _
                                                       .ExpBarAverage = Math.Max(eBar, 50), _
                                                       .IGen = New AllDataStructues.LootGenSettings(False), _
                                                       .LeaderModificators = New List(Of String), _
                                                       .UnitsModificators = New List(Of String)}
    End Function

End Class

Public Class RaceGen

    Dim comm As Common
    Dim rndgen As New RndValueGen
    Dim symm As New SymmetryOperations

    Public Const WaterRace As Integer = 11

    'Private commonBlock As String = "D," & "W," & "A,AS,AW,A+AW+AG"
    ''Private commonBlock As String = "D,D+A+AW+AG," & "W," & "A,AS,AW,A+AW+AG"
    '
    'Private LocRaces() As String = New String() {"H:2:H,H+AW," & commonBlock, _
    '                                             "U:2:U,U+AS," & commonBlock, _
    '                                             "L:2:L," & commonBlock, _
    '                                             "C:2:C,C+AST,AST,AST+AW," & commonBlock, _
    '                                             "E:2:E,E+A+AW+AG,E+AG," & commonBlock, _
    '                                             "N:3:N,G,B,G+AW+AS,B+AW," & commonBlock, _
    '                                             "S:1:S,S+A,S+AS," & commonBlock}
    'Dim StackRaceWeight() As Double = New Double() {0, 1, 1, 1, 1, 0.75, 1, 1, 2, 0.05, 1, 1, 1, 1, 1, 1, 0.75, 0, 1, 1}
    ''                                               -, H, U, L, C,    N, H, E, G,    D, S, W, B, A, E,AS,  AST, -. AW,AG 

    Dim LRaces() As Integer
    Dim LRacesWeight(), SRacesWeight()() As Double
    Dim SRaces()()() As Integer
    ReadOnly neutralI As Integer = -1

    Public Sub New(ByRef c As Common)
        comm = c
        ReDim LRaces(UBound(comm.defValues.LocRacesBlocks)), LRacesWeight(UBound(comm.defValues.LocRacesBlocks))
        Dim m As Integer = 0
        Dim tmpRaceWeight As New Dictionary(Of String, Double)
        For i As Integer = 0 To UBound(comm.defValues.LocRacesBlocks) Step 1
            Dim ch As String = comm.defValues.LocRacesBlocks(i).Split(CChar(":"))(0)
            LRaces(i) = comm.RaceIdentifierToSubrace(ch)
            m = Math.Max(m, LRaces(i))
            If ch.ToUpper = "N" Then neutralI = i
        Next i
        For i As Integer = 0 To UBound(comm.defValues.LocRacesBlocks) Step 1
            LRacesWeight(i) = comm.RaceIdentifierToSubrace(comm.defValues.LocRacesBlocks(i).Split(CChar(":"))(1))
        Next i
        ReDim SRaces(m), SRacesWeight(m)
        For i As Integer = 0 To UBound(comm.defValues.LocRacesBlocks) Step 1
            Dim races() As String = comm.defValues.LocRacesBlocks(i).Split(CChar(":"))(2).Split(CChar(","))
            ReDim SRaces(LRaces(i))(UBound(races)), SRacesWeight(LRaces(i))(UBound(races))
            For j As Integer = 0 To UBound(races) Step 1
                Dim s() As String = races(j).Split(CChar("+"))
                ReDim SRaces(LRaces(i))(j)(UBound(s))
                For k As Integer = 0 To UBound(s) Step 1
                    Dim rI As Integer = comm.RaceIdentifierToSubrace(s(k))
                    SRaces(LRaces(i))(j)(k) = rI
                    SRacesWeight(LRaces(i))(j) += comm.defValues.StackRaceChance(rI)
                    If Not tmpRaceWeight.ContainsKey(s(k).ToUpper) Then tmpRaceWeight.Add(s(k).ToUpper, comm.defValues.StackRaceChance(rI))
                Next k
                SRacesWeight(LRaces(i))(j) /= SRaces(LRaces(i))(j).Length
                Array.Sort(SRaces(LRaces(i))(j))
            Next j
        Next i
        tmpRaceWeight.Clear()
    End Sub

    '''<summary>Сгенерирует для каждой локации и каждого отряда допустимые расы</summary>
    ''' <param name="m">Заготовка карты после работы генератора положения отрядов и их силы</param>
    ''' <param name="PlayableRaces">За какие расы будем играть. Если Nothing, то расы будут сгенерированы</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций</param>
    Public Sub Gen(ByRef m As Map, ByRef PlayableRaces() As Integer, _
                   ByRef settRaceLoc As Map.SettingsLoc, ByVal settCommLoc As Map.SettingsLoc)
        Dim a() As Map.SettingsLoc = Map.SettingsLoc.ToArray(settRaceLoc, settCommLoc, RacesAmount(m), m.Loc.Length)
        Call Gen(m, PlayableRaces, a)
    End Sub

    '''<summary>Сгенерирует для каждой локации и каждого отряда допустимые расы</summary>
    ''' <param name="m">Заготовка карты после работы генератора положения отрядов и их силы</param>
    ''' <param name="PlayableRaces">За какие расы будем играть. Если Nothing, то расы будут сгенерированы</param>
    ''' <param name="settLoc">Настройки для каждой локации. Первыми должны идти стартовые локации рас.</param>
    Public Sub Gen(ByRef m As Map, ByRef PlayableRaces() As Integer, ByRef settLoc() As Map.SettingsLoc)

        If Not m.complited.WaterCreation_Done Then
            Throw New Exception("Сначала нужно выполнить WaterGen.Gen")
        End If

        Dim t0 As Integer = Environment.TickCount
        Dim nRaces As Integer = RacesAmount(m)
        Dim LocR() As Integer = GenLocRace(m, nRaces, PlayableRaces, settLoc)
        Call SetLocRaceToCells(m, LocR, nRaces, settLoc)

        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).stack.GuardLoc Or m.board(x, y).stack.PassGuardLoc Then
                    Dim group As Integer = m.board(x, y).groupID
                    If m.groupStats.Item(group).Race.Count = 0 Then 'AndAlso Not IsNothing(m.board(x, y).stack.stackRace) Then
                        For Each r As Integer In m.board(x, y).stack.stackRace
                            m.groupStats.Item(group).Race.Add(r)
                        Next r
                        If m.groupStats.ContainsKey(-group) Then
                            For Each r As Integer In m.board(x, y).stack.stackRace
                                m.groupStats.Item(-group).Race.Add(r)
                            Next r
                        End If
                    End If
                End If
            Next x
        Next y

        For Each key As Integer In m.groupStats.Keys
            If IsNothing(m.groupStats.Item(key).Race) OrElse m.groupStats.Item(key).Race.Count = 0 Then
                Throw New Exception("Группа с ID " & key & " не имеет расы")
            End If
        Next key
        m.complited.StacksRaceGen_Done = True
        Call m.log.Add("Stacks race generation: " & Environment.TickCount - t0 & " ms")
    End Sub

    Private Function RacesAmount(ByRef m As Map) As Integer
        Dim n As Integer = 0
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).passability.isAttended AndAlso m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then n += 1
            Next x
        Next y
        Return n
    End Function
    Private Function GenLocRace(ByRef m As Map, ByRef nRaces As Integer, ByRef PlayableRaces() As Integer, ByRef settLoc() As Map.SettingsLoc) As Integer()
        'выбор играбельных рас
        Dim selectedRaces As New List(Of Integer)
        If IsNothing(PlayableRaces) Then
            Dim AllPlayableRacesList(UBound(LRaces)) As Integer
            Dim n As Integer = -1
            For i As Integer = 0 To UBound(comm.defValues.playableRaces) Step 1
                Dim id As Integer = comm.RaceIdentifierToSubrace(comm.defValues.playableRaces(i))
                For j As Integer = 0 To UBound(LRaces) Step 1
                    If id = LRaces(j) Then
                        n += 1
                        AllPlayableRacesList(n) = j
                        Exit For
                    End If
                Next j
            Next i
            ReDim Preserve AllPlayableRacesList(n)
            selectedRaces.AddRange(AllPlayableRacesList)
            For i As Integer = 0 To UBound(AllPlayableRacesList) - nRaces Step 1
                Dim s As Integer = comm.RandomSelection(selectedRaces, True)
                selectedRaces.Remove(s)
            Next i
        Else
            selectedRaces.AddRange(PlayableRaces)
            If PlayableRaces.Length < nRaces Then
                For i = PlayableRaces.Length + 1 To nRaces Step 1
                    selectedRaces.Add(GenDefaultValues.randomRaceID)
                Next i
            ElseIf PlayableRaces.Length > nRaces Then
                Do While selectedRaces.Count > nRaces
                    selectedRaces.RemoveAt(rndgen.RndInt(0, selectedRaces.Count - 1, True))
                Loop
            End If
        End If
        If Not selectedRaces.Count = nRaces Then
            Throw New Exception("Количество рас не соответствует количеству столиц на карте")
            Return Nothing
        End If
        Do While selectedRaces.Contains(GenDefaultValues.randomRaceID)
            Dim s As New List(Of Integer)
            For Each r As String In comm.defValues.playableRaces
                Dim subrace As Integer = comm.RaceIdentifierToSubrace(r)
                If Not selectedRaces.Contains(subrace) Then
                    s.Add(subrace)
                End If
            Next r
            selectedRaces.Remove(GenDefaultValues.randomRaceID)
            selectedRaces.Add(comm.RandomSelection(s, True))
        Loop
        Dim LocR(UBound(m.Loc)) As Integer
        For i As Integer = 1 To nRaces Step 1
            If IsNothing(PlayableRaces) Then
                Dim s As Integer = comm.RandomSelection(selectedRaces, True)
                selectedRaces.Remove(s)
                LocR(i - 1) = LRaces(s)
            Else
                LocR(i - 1) = selectedRaces.Item(i - 1)
            End If
        Next i

        'выбор рас для обычных локаций
        Dim ids As New List(Of Integer)
        For i As Integer = nRaces To UBound(LocR) Step 1
            If Not m.Loc(i).IsObtainedBySymmery Then
                ids.Clear()
                If IsNothing(settLoc(i).possibleRaces) Then
                    For j As Integer = 0 To UBound(LRaces) Step 1
                        ids.Add(j)
                    Next j
                Else
                    For Each race As String In settLoc(i).possibleRaces
                        Dim rID As Integer = comm.RaceIdentifierToSubrace(race, False)
                        For j As Integer = 0 To UBound(LRaces) Step 1
                            If rID = LRaces(j) Then
                                ids.Add(j)
                                Exit For
                            End If
                        Next j
                    Next race
                    If ids.Count = 0 Then
                        For j As Integer = 0 To UBound(LRaces) Step 1
                            ids.Add(j)
                        Next j
                    End If
                End If
                Dim R As Integer = LRaces(comm.RandomSelection(ids, LRacesWeight, True))
                If m.symmID > -1 Then
                    Dim pp() As Point = symm.ApplySymm(m.Loc(i).pos, nRaces, m, 1)
                    For Each p As Point In pp
                        Dim id As Integer = Location.FindLocIDByPosition(m, p)
                        LocR(id) = R
                    Next p
                Else
                    LocR(i) = R
                End If
            End If
        Next i
        Return LocR
    End Function
    Private Sub SetLocRaceToCells(ByRef m As Map, ByRef LocR() As Integer, ByRef nRaces As Integer, ByRef settLoc() As Map.SettingsLoc)

        For i As Integer = 0 To UBound(LocR) Step 1
            m.Loc(i).Race = LocR(i)
        Next i

        Dim t As Integer = 0
        For Each item As Integer()() In SRaces
            If Not IsNothing(item) Then t += item.Length
        Next item
        Dim races(t - 1) As List(Of Integer)
        For i As Integer = 0 To UBound(races) Step 1
            races(i) = New List(Of Integer)
        Next i
        Dim weight(t - 1) As Double

        For NLoop As Integer = 0 To 1 Step 1
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If IsNothing(m.board(x, y).stack.stackRace) And (Not m.Loc(m.board(x, y).locID(0) - 1).IsObtainedBySymmery Or NLoop = 1) Then
                        Call SetCellRace(m, LocR, nRaces, races, weight, x, y, settLoc)
                    End If
                Next x
            Next y
        Next NLoop

        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                m.board(x, y).mapObject.objRace = New List(Of Integer)
                For Each r As Integer In m.board(x, y).locID
                    If Not m.board(x, y).mapObject.objRace.Contains(LocR(r - 1)) Then m.board(x, y).mapObject.objRace.Add(LocR(r - 1))
                Next r
            Next x
        Next y

    End Sub
    Private Sub SetCellRace(ByRef m As Map, ByRef LocR() As Integer, ByRef nRaces As Integer, ByRef races() As List(Of Integer), _
                            ByRef weight() As Double, ByRef x As Integer, ByRef y As Integer, ByRef settLoc() As Map.SettingsLoc)
        Dim rO, rS, IDs As New List(Of Integer)
        Dim added As New List(Of String)
        Dim t As Integer = -1
        Dim isWaterCell As Boolean = MayBeWater(m, x, y)

        If Not isWaterCell Then
            For Each r As Integer In m.board(x, y).locID
                If Not rO.Contains(LocR(r - 1)) Then rO.Add(LocR(r - 1))
            Next r
            If m.board(x, y).locID(0) > nRaces Then
                For Each r As Integer In rO
                    For i As Integer = 0 To UBound(SRaces(r)) Step 1
                        Dim str As String = ""
                        For q As Integer = 0 To UBound(SRaces(r)(i)) Step 1
                            str &= SRaces(r)(i)(q).ToString & "_"
                        Next q
                        If str = WaterRace & "_" AndAlso Not added.Contains(str) Then
                            If Not isWaterCell Then added.Add(str)
                        End If
                        If Not added.Contains(str) Then
                            Call AddPossibleRace(t, r, i, races, weight)
                            added.Add(str)
                        End If
                    Next i
                Next r
            Else
                Dim baseNeutralStacksRace As Integer
                If IsNothing(settLoc(m.board(x, y).locID(0) - 1).possibleRaces) Then
                    baseNeutralStacksRace = neutralI
                Else
                    Dim possibleRacesList As New List(Of Integer)
                    For Each race As String In settLoc(m.board(x, y).locID(0) - 1).possibleRaces
                        Dim rID As Integer = comm.RaceIdentifierToSubrace(race, False)
                        For j As Integer = 0 To UBound(LRaces) Step 1
                            If rID = LRaces(j) Then
                                possibleRacesList.Add(j)
                                Exit For
                            End If
                        Next j
                    Next race
                    If possibleRacesList.Count = 0 Then
                        For j As Integer = 0 To UBound(LRaces) Step 1
                            possibleRacesList.Add(j)
                        Next j
                    End If
                    baseNeutralStacksRace = comm.RandomSelection(possibleRacesList, LRacesWeight, True)
                    possibleRacesList.Clear()
                End If
                Dim add As Boolean
                For i As Integer = 0 To UBound(SRaces(baseNeutralStacksRace)) Step 1
                    add = True
                    If SRaces(baseNeutralStacksRace)(i).Length = 1 AndAlso SRaces(baseNeutralStacksRace)(i)(0) = WaterRace Then add = isWaterCell
                    If add Then Call AddPossibleRace(t, baseNeutralStacksRace, i, races, weight)
                Next i
            End If
            IDs.Clear()
            For i As Integer = 0 To t Step 1
                IDs.Add(i)
            Next i
            Dim s As Integer = comm.RandomSelection(IDs, weight, True) ' rndgen.RndPos(t + 1, True) - 1

            For Each item As Integer In races(s)
                rS.Add(item)
            Next item
        Else
            rS.Add(WaterRace)
        End If
        If m.symmID > -1 Then
            Dim pp() As Point = symm.ApplySymm(New Point(x, y), nRaces, m, 1)
            For Each p As Point In pp
                m.board(p.X, p.Y).stack.stackRace = New List(Of Integer)
                For Each r As Integer In rS
                    m.board(p.X, p.Y).stack.stackRace.Add(r)
                Next r
            Next p
        Else
            m.board(x, y).stack.stackRace = New List(Of Integer)
            For Each r As Integer In rS
                m.board(x, y).stack.stackRace.Add(r)
            Next r
        End If
    End Sub
    Private Sub AddPossibleRace(ByRef t As Integer, raceLocID As Integer, ByRef i As Integer, _
                                ByRef races() As List(Of Integer), ByRef weight() As Double)
        t += 1
        races(t).Clear()
        weight(t) = SRacesWeight(raceLocID)(i)
        For q As Integer = 0 To UBound(SRaces(raceLocID)(i))
            races(t).Add(SRaces(raceLocID)(i)(q))
        Next q
    End Sub
    Friend Shared Function MayBeWater(ByRef m As Map, ByRef x As Integer, ByRef y As Integer) As Boolean
        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
        For q As Integer = b.minY To b.maxY Step 1
            For p As Integer = b.minX To b.maxX Step 1
                If Not m.board(p, q).surface.isWater Then Return False
            Next p
        Next q
        Return True
    End Function
End Class