Public Class SetName

    Private link As String = "https://norvezskayasemga.wixsite.com/d2bfwmod/donaters"
    Private path As String = ".\Donaters.txt"
    Private users() As Donater = Nothing
    Private comm As Common
    Private rndgen As New RndValueGen
    Private exclude As New List(Of String)
    Private excludeRace As New List(Of Integer)
    Friend Const StackNameMaxLen As Integer = 30
    Friend Const LordNameMaxLen As Integer = 15
    Friend Const CityNameMaxLen As Integer = 20
    Friend Const CapitalNameMaxLen As Integer = 20
    Friend Const MerchantNameMaxLen As Integer = 35
    Friend Const RuinsNameMaxLen As Integer = 25
    Friend Const DescriptionMaxLen As Integer = 100
    Private commonIDs, lordIDs As RandomSelection
    Private defaultLords As New Dictionary(Of Integer, RandomSelection)
    Private LordMinWeight As Double
    Private defaultLordNames(14)() As String
    Private lordDonationThreshold As Double = 999
    Public customObjectsNames As GenDefaultValues.MapObjectsText
    Public preferedUnit As New Dictionary(Of String, String)
    Private Const chanceToAddNameToStack As Double = 0.15 ' (0,1]

    Private Const DesiredUnitWeightMultiplier As Double = 3
    Private Const DesiredUnitMinWeightMultiplier As Double = 0.5
    Private ReadOnly NoneNameId As Integer = -1

    ''' <summary>Сюда генератор пишет лог</summary>
    Public log As Log
    Private initializationLog As String

    Private Structure Donater
        Dim name As String
        Dim unit As String
        Dim weight As Double

        Dim increasedWeight As Double
        Dim decreasedWeight As Double
        Dim unitsCount As Integer
    End Structure

#Region "Constructor"
    Public Sub New(ByVal lang As GenDefaultValues.TextLanguage, ByVal modName As String)
        comm = New Common(modName)
        log = New Log(comm)

        customObjectsNames = New GenDefaultValues.MapObjectsText(lang, comm.defValues)

        Call log.Enable()
        Call AddToLog(-1, "-----Names creator initialization started-----")

        For Each k As String In comm.LordsRace.Keys
            If IsNothing(defaultLordNames(comm.LordsRace.Item(k))) Then
                defaultLordNames(comm.LordsRace.Item(k)) = customObjectsNames.LordNamesArray(k)
            End If
        Next k

        Dim t As Date = ReadNames()
        Dim download As Boolean = False
        If IsNothing(users) Then
            download = True
        Else
            Dim d As Integer = 365 * (DateAndTime.Today.Year - t.Year) + DateAndTime.Today.DayOfYear - t.DayOfYear
            If d > 7 Then download = True
        End If
        If download Then
            Call DownloadList()
            Call PrintNames()
        End If
        Call ReadExclusions()

        ReDim Preserve users(users.Length)
        NoneNameId = UBound(users)
        users(NoneNameId) = New Donater With {.name = "!!!NameBug!!!", .unit = "", .weight = 0}
        For i As Integer = 0 To UBound(users) - 1 Step 1
            users(NoneNameId).weight += users(i).weight
        Next i
        users(NoneNameId).weight *= NoneWeightMultiplier()
        commonIDs = New RandomSelection(users.Length, rndgen)
        lordIDs = New RandomSelection(users.Length, rndgen)

        Call AddToLog(-1, "-----Names creator initialization ended-----")
        If Not IsNothing(log) Then initializationLog = log.PrintAll
    End Sub
    Private Shared Function NoneWeightMultiplier() As Double
        Return 1 / chanceToAddNameToStack - 1
    End Function

    Private Sub DownloadList()

        Dim w As System.Net.WebClient = Nothing
        Dim TableContent()() As String = Nothing
        Try
            Net.ServicePointManager.SecurityProtocol = CType(3072, Net.SecurityProtocolType) Or _
                                                       CType(768, Net.SecurityProtocolType) Or _
                                                       CType(192, Net.SecurityProtocolType) Or _
                                                       CType(48, Net.SecurityProtocolType)

            w = New System.Net.WebClient With {.Proxy = Nothing}
            Dim byteData() As Byte = w.DownloadData(link)
            Dim d As String = Text.Encoding.UTF8.GetString(byteData)

            Dim str() As String = d.Replace(Chr(10), vbNewLine).Split(CChar(vbNewLine))
            Dim base As String = ""
            For s As Integer = 0 To UBound(str) Step 1
                If str(s).Contains("!-- warmup data start") Then
                    base = str(s + 1)
                    Exit For
                End If
            Next s
            If base = "" Then
                w.Dispose()
                Exit Sub
            End If
            str = base.Split(CChar("<"))
            Dim tableRow As Integer = 0
            Dim tableColumn As Integer = 0
            Dim columnsCount As Integer = 4
            Dim table As String = ""
            For i As Integer = 0 To UBound(str) Step 1
                If Not str(i).StartsWith("span class=\") Then
                    str(i) = ""
                Else
                    str(i) = str(i).Substring(str(i).IndexOf(">") + 1)
                    table &= str(i)
                    tableColumn += 1
                    If tableColumn = columnsCount Then
                        tableRow += 1
                        tableColumn = 0
                        table &= vbNewLine
                    Else
                        table &= " "
                    End If
                End If
            Next i
            If tableRow > 0 Then
                ReDim TableContent(tableRow - 1)
                tableRow = 0
                tableColumn = 0
                For i As Integer = 0 To UBound(str) Step 1
                    If Not str(i) = "" Then
                        tableColumn += 1
                        If tableColumn = 1 Then ReDim TableContent(tableRow)(columnsCount - 1)
                        TableContent(tableRow)(tableColumn - 1) = str(i)
                        If tableColumn = columnsCount Then
                            tableRow += 1
                            tableColumn = 0
                        End If
                    End If
                Next i
            End If
        Catch ex As Exception
            If GenDefaultValues.writeToConsole Then Console.WriteLine(ex.Message)
            AddToLog(-1, "Names downloader: " & ex.Message)
        End Try
        If IsNothing(TableContent) Then
            Dim t As String = _
            "Патрик+0.123702668266555+g000000000" & vbNewLine & _
            "rus4661+0.123702668266555+g000000000" & vbNewLine & _
            "mak+1.23702668266555+g000000000" & vbNewLine & _
            "Олесь Гайдуков+1.26795234973218+g000000000" & vbNewLine & _
            "Вячеслав+0.556662007199495+g000000000" & vbNewLine & _
            "Гласан+0.154628335333193+g000000000" & vbNewLine & _
            "Григорий Львов+0.618513341332773+g000000000" & vbNewLine & _
            "DaryaDEMO+0.0494810673066218+g000000000" & vbNewLine & _
            "Вадим Цымбал+0.742216009599327+g000000000" & vbNewLine & _
            "Eker+0.123702668266555+g000000000" & vbNewLine & _
            "Vilgeforc+0.989621346132436+g000000000" & vbNewLine & _
            "CetusSantis+0.278331003599748+g000000000" & vbNewLine & _
            "Андрей+0.123702668266555+g000000000" & vbNewLine & _
            "ОторвиГолову+2.47405336533109+g000000000" & vbNewLine & _
            "Котейка+2.42457229802447+g000000000" & vbNewLine & _
            "Rock Wolf+0.247405336533109+g000000000" & vbNewLine & _
            "Руслан+0.123702668266555+g000000000" & vbNewLine & _
            "Grog+0.123702668266555+g000000000" & vbNewLine & _
            "Maksoncheggg+0.123702668266555+g000000000" & vbNewLine & _
            "FIdGer10rus+0.00618513341332773+g000000000" & vbNewLine & _
            "Илья+0.123702668266555+g000000000" & vbNewLine & _
            "Semga+0.123702668266555+g000uu8268" & vbNewLine & _
            "Gufi Sadness+0.593772807679462+g000000000" & vbNewLine & _
            "Берлога Зверя+0.0309256670666386+g000000000" & vbNewLine & _
            "Константин+0.123702668266555+g000000000" & vbNewLine & _
            "Kvetzakoml+0.618513341332773+g000000000" & vbNewLine & _
            "Nazar+0.0309256670666386+g000000000" & vbNewLine & _
            "EugenScorpion+0.618513341332773+g000uu5108" & vbNewLine & _
            "Zeriosis+0.432959338932941+g000000000" & vbNewLine & _
            "Rico+1.97924269226487+g000000000" & vbNewLine & _
            "Sned+0.154628335333193+g000000000" & vbNewLine & _
            "Exactor+0.618513341332773+g000000000" & vbNewLine & _
            "Nexx Darkholme+0.927770011999159+g000000000" & vbNewLine & _
            "Даниил+0.123702668266555+g000000000" & vbNewLine & _
            "AstralLein+0.247405336533109+g000uu8257" & vbNewLine & _
            "Sergey Belokur+1.1380645480523+g000uu2023" & vbNewLine & _
            "xstokx(popLimfo)+0.618513341332773+g000000000" & vbNewLine & _
            "Anatoly Levchik+2.85753163695741+g000000000" & vbNewLine & _
            "Виктор+0.123702668266555+g000000000" & vbNewLine & _
            "Светлаха+0.247405336533109+g000000000" & vbNewLine & _
            "Артемиус+0.123702668266555+g000000000" & vbNewLine & _
            "Сергей+0.123702668266555+g000000000" & vbNewLine & _
            "Pavel+0.123702668266555+g000000000" & vbNewLine & _
            "Андрей Копылов+0.927770011999159+g000000000" & vbNewLine & _
            "SpyroTF+0.371108004799664+g000000000" & vbNewLine & _
            "Dustik+0.0618513341332773+g000000000" & vbNewLine & _
            "Nameless Raven+0.0618513341332773+g000000000" & vbNewLine & _
            "Sunshinetsoy+1.23702668266555+g000000000" & vbNewLine & _
            "Wiktorderelf+0.123702668266555+g000000000" & vbNewLine & _
            "Злой Виталик+0.432959338932941+g000000000" & vbNewLine & _
            "Кирь+0.371108004799664+g000000000" & vbNewLine & _
            "Findme+3.21626937493042+g000uu5126" & vbNewLine & _
            "Efremov+18.5554002399832+g000000000" & vbNewLine & _
            "tyrgor+0.123702668266555+g000000000" & vbNewLine & _
            "Father Patrick+0.0618513341332773+g000000000" & vbNewLine & _
            "Leonik+0.0618513341332773+g000000000" & vbNewLine & _
            "Feduan+0.123702668266555+g000000000" & vbNewLine & _
            "MerovingianOff+0.402033671866302+g000uu8208" & vbNewLine & _
            "Ave_Mne!+0.618513341332773+g000000000" & vbNewLine & _
            "NornaGest+0.296886403839731+g000000000" & vbNewLine & _
            "Leo+0.123702668266555+g000000000" & vbNewLine & _
            "Кирилл САН+0.0865918677865882+g000000000" & vbNewLine & _
            "BiTAyT+6.92734942292705+g000000000" & vbNewLine & _
            "Rave+1.23702668266555+g000000000" & vbNewLine & _
            "FC Bayern+0.0804067343732604+g000000000" & vbNewLine & _
            "Мотлин+0.556662007199495+g000000000" & vbNewLine & _
            "Team+0.123702668266555+g000000000" & vbNewLine & _
            "XAOC+0.0618513341332773+g000000000" & vbNewLine & _
            "Mirown+1.23702668266555+g000000000" & vbNewLine & _
            "Andary+1.75534086270241+g000000000" & vbNewLine & _
            "НеНормал+0.123702668266555+g000000000" & vbNewLine & _
            "Roger Fun+0.0432959338932941+g000000000" & vbNewLine & _
            "Honests Pony+0.247405336533109+g000000000" & vbNewLine & _
            "UnveN+1.23702668266555+g000000000" & vbNewLine & _
            "Risemyself+0.185554002399832+g000000000" & vbNewLine & _
            "Bollocks+12.1228614901223+g000000000" & vbNewLine & _
            "Diiwane Crew+21.0294536053143+g000000000" & vbNewLine & _
            "lentiX+1.38546988458541+g000000000" & vbNewLine & _
            "Александр Хлынов+0.371108004799664+g000000000"

            Dim splited() As String = t.Replace(Chr(13), Chr(10)).Replace(Chr(10) & Chr(10), Chr(10)).Split(Chr(10))
            ReDim TableContent(UBound(splited))
            Dim s() As String
            For i As Integer = 0 To UBound(splited) Step 1
                s = splited(i).Split(CChar("+"))
                If Not UBound(s) = 2 Then Throw New Exception("Invalid columns count")
                ReDim TableContent(i)(3)
                TableContent(i)(0) = s(0)
                TableContent(i)(1) = s(1)
                TableContent(i)(2) = "0"
                TableContent(i)(3) = s(2)
            Next i
        End If
        Try
            If Not IsNothing(TableContent) Then
                Dim added As New Dictionary(Of String, Integer)
                ReDim users(UBound(TableContent))
                For i As Integer = 0 To UBound(TableContent) Step 1
                    Dim name As String = TableContent(i)(0)
                    Dim lineWeight As Double = CDbl(TableContent(i)(1)) + CDbl(TableContent(i)(2))
                    Dim prefID As String = TableContent(i)(3)
                    If IsNothing(prefID) OrElse prefID = "" Then prefID = GenDefaultValues.emptyItem
                    prefID = prefID.ToUpper
                    If Not IsNothing(name) AndAlso Not name = "" AndAlso lineWeight > 0 Then
                        Dim s As String = name.ToUpper
                        If Not added.ContainsKey(s) Then
                            users(added.Count).name = name
                            users(added.Count).unit = ""
                            added.Add(s, added.Count)
                        End If
                        users(added.Item(s)).weight = Math.Max(users(added.Item(s)).weight, lineWeight)
                        If users(added.Item(s)).unit = "" Or users(added.Item(s)).unit.ToUpper = GenDefaultValues.emptyItem.ToUpper Then users(added.Item(s)).unit = prefID
                    End If
                Next i
                If UBound(users) > added.Count - 1 Then ReDim Preserve users(added.Count - 1)
                Dim wsum As Double = 0
                For i As Integer = 0 To UBound(users) Step 1
                    wsum += users(i).weight
                Next i
                For i As Integer = 0 To UBound(users) Step 1
                    users(i).weight /= wsum
                Next i
                LordMinWeight = lordDonationThreshold / wsum
            End If
        Catch ex As Exception
            If GenDefaultValues.writeToConsole Then Console.WriteLine(ex.Message)
            AddToLog(-1, "Names weight sum: " & ex.Message)
        End Try
        Try
            If Not IsNothing(w) Then w.Dispose()
        Catch ex As Exception
            AddToLog(-1, "Web client dispose: " & ex.Message)
        End Try
    End Sub

    Private Sub PrintNames()
        Try
            If IsNothing(users) Then Exit Sub
            Dim str As String = LordMinWeight.ToString
            For i As Integer = 0 To UBound(users) Step 1
                If Not i = NoneNameId Then
                    str &= vbNewLine & users(i).name & vbTab & users(i).weight & vbTab & users(i).unit
                End If
            Next i
            AddToLog(-1, "Writing " & path)
            IO.File.WriteAllText(path, str)
            AddToLog(-1, "End writing")
        Catch ex As Exception
            AddToLog(-1, ex.Message)
        End Try
    End Sub
    Private Function ReadNames() As Date
        If Not IO.File.Exists(path) Then Return Nothing
        Dim t As Date = Nothing
        AddToLog(-1, "Reading " & path)
        Try
            Dim alltext As String = IO.File.ReadAllText(path)
            AddToLog(-1, "Content:" & vbNewLine & alltext & vbNewLine & "-------------------")
            Dim str() As String = Common.TxtSplit(alltext)
            Dim s(), w As String
            Dim i0 As Integer
            If IsNumeric(str(0)) Then
                AddToLog(-1, "Lord min weight found")
                i0 = 1
            Else
                AddToLog(-1, "Lord min weight not found (don't worry)")
                i0 = 0
            End If
            ReDim users(UBound(str) - i0)
            Dim wmax As Double
            For i As Integer = i0 To UBound(users) + i0 Step 1
                s = str(i).Split(CChar(" "))
                For j As Integer = 0 To UBound(s) - 2 Step 1
                    users(i - i0).name &= s(j)
                    If j < UBound(s) - 1 Then users(i - i0).name &= " "
                Next j
                w = s(UBound(s) - 1)
                users(i - i0).unit = s(UBound(s))
                AddToLog(-1, "Read line " & i + 1 & "; Name: " & users(i - i0).name & " weight: " & w & " unit: " & users(i - i0).unit)
                users(i - i0).weight = ValueConverter.StrToDbl(w)
                wmax = Math.Max(wmax, users(i - i0).weight)
            Next i
            If IsNumeric(str(0)) Then
                LordMinWeight = ValueConverter.StrToDbl(str(0))
            Else
                LordMinWeight = 0.49 * wmax
            End If
            AddToLog(-1, "LordMinWeight = " & LordMinWeight)
            t = IO.File.GetCreationTime(path)
        Catch ex As Exception
            AddToLog(-1, ex.Message)
        End Try
        AddToLog(-1, "End reading")
        Return t
    End Function
    Private Sub ReadExclusions()
        Try
            excludeRace.Add(comm.RaceIdentifierToSubrace("Greenskins"))
            Dim str() As String = Common.TxtSplit(comm.defValues.resReader.ExcludeIDsForNames)
            log.Add(comm.ReadingLog.PrintAll)
            For i As Integer = 0 To UBound(str) Step 1
                exclude.Add(str(i).ToUpper)
            Next i
        Catch ex As Exception
            If GenDefaultValues.writeToConsole Then Console.WriteLine(ex.Message)
            AddToLog(-1, "Exclusions reader: " & ex.Message)
        End Try
    End Sub
#End Region
#Region "Logging"
    Private Sub AddToLog(ByRef LogID As Integer, ByRef Msg As String)
        If LogID > -1 Then
            Call log.MAdd(LogID, Msg)
        Else
            Call log.Add(Msg)
        End If
    End Sub
    Private Sub ClearLog(ByRef LogID As Integer)
        If LogID > -1 Then
            Call log.MClear(LogID)
        Else
            Call log.Clear()
        End If
    End Sub
#End Region

#Region "Stacks"
    '''<summary>Присвоит имя отрядам</summary>
    ''' <param name="stack">Уже сгенерированные стэки</param>
    ''' <param name="R">Инициализированный класс</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Sub GenName(ByRef stack() As AllDataStructues.Stack, ByRef R As RandStack, ByVal newMapGen As Boolean, _
                       Optional ByVal LogID As Integer = -1)
        If IsNothing(stack) Or IsNothing(R) Then Exit Sub
        Call ResetNames(newMapGen, LogID)

        Dim possibleNames As New List(Of String)
        Dim leaderID(UBound(stack)) As String
        Dim skip(UBound(stack)), SkippedByUnitType(UBound(stack)) As Boolean
        Dim ids(UBound(stack)), i As Integer
        For k As Integer = 0 To UBound(stack) Step 1
            ids(k) = k
        Next k
        Call rndgen.Shuffle(ids)

        Dim uCount As New Dictionary(Of String, Integer())
        For k As Integer = 0 To UBound(stack) Step 1
            i = ids(k)
            If Not IsNothing(stack(i)) Then
                If stack(i).leaderPos > -1 AndAlso Not IsNothing(stack(i).units) Then
                    Dim unitID As String = stack(i).units(stack(i).leaderPos).unit.unitID.ToUpper
                    If Not uCount.ContainsKey(unitID) Then uCount.Add(unitID, New Integer() {0})
                    uCount.Item(unitID)(0) += 1
                Else
                    stack(i).name = ""
                    skip(i) = True
                End If
            Else
                skip(i) = True
            End If
        Next k
        Dim totalNonskipped, totalNonskippedByUnitType As Integer
        For k As Integer = 0 To UBound(stack) Step 1
            i = ids(k)
            If Not skip(i) Then
                totalNonskipped += 1
                SkippedByUnitType(i) = True
                For Each id As Integer In commonIDs
                    If Not SkipUnit(id, stack(i).units(stack(i).leaderPos).unit) Then
                        SkippedByUnitType(i) = False
                        totalNonskippedByUnitType += 1
                        Exit For
                    End If
                Next id
            End If
        Next k
        Call calcUWeigt(uCount)

        Dim wMultiplier As Double = totalNonskipped / Math.Max(totalNonskippedByUnitType, 1)
        For k As Integer = 0 To UBound(stack) Step 1
            i = ids(k)
            If Not skip(i) And Not SkippedByUnitType(i) Then
                stack(i).name = SetName(stack(i).units(stack(i).leaderPos).unit, wMultiplier, LogID)
                skip(i) = True
            End If
            If stack(i).leaderPos > -1 AndAlso IsNothing(stack(i).name) OrElse stack(i).name = "" Then
                stack(i).name = stack(i).units(stack(i).leaderPos).unit.name
            End If
        Next k
    End Sub
    Private Function SetName(ByRef leader As AllDataStructues.Unit, _
                             ByVal UserWeightMultiplier As Double, ByRef LogID As Integer) As String
        Dim res As String = leader.name
        Dim i As Integer = commonIDs.RandomSelection(WeightArray(UserWeightMultiplier, leader.unitID))
        If i = NoneNameId Then Return res

        If SkipUnit(i, leader) Then Return res

        commonIDs.Remove(i)
        res &= " " & users(i).name
        res = NameCut(res, StackNameMaxLen)
        AddToLog(LogID, "Using name: " & users(i).name)
        Return res
    End Function
    Private Function SkipUnit(ByRef userID As Integer, ByRef unit As AllDataStructues.Unit) As Boolean
        If Not unit.unitID.ToUpper = users(userID).unit.ToUpper Then
            If excludeRace.Contains(unit.race) Then Return True
            If exclude.Contains(unit.unitID) Then Return True
        End If
        Return False
    End Function
    Private Function NameCut(ByRef input As String, ByRef maxLen As Integer) As String
        If input.Length > maxLen Then
            Return input.Substring(0, maxLen)
        Else
            Return input
        End If
    End Function
#End Region
#Region "Lords"
    '''<summary>Присвоит имя лорду</summary>
    ''' <param name="RaceID">ID расы лорда</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function LordName(ByRef RaceID As Integer, ByVal newMapGen As Boolean, _
                             Optional ByVal LogID As Integer = -1) As String
        Call ResetNames(newMapGen, LogID)

        Dim result As String
        If lordIDs.Count > 0 AndAlso rndgen.RndDbl(0, 1) > 0.5 Then
            Dim i As Integer = lordIDs.RandomSelection(WeightArray)
            lordIDs.Remove(i)
            result = users(i).name
        Else
            If defaultLords.Item(RaceID).Count = 0 Then Throw New Exception("Lords names list is empty. Race ID: " & RaceID)
            Dim i As Integer = defaultLords.Item(RaceID).RandomSelection()
            defaultLords.Item(RaceID).Remove(i)
            result = defaultLordNames(RaceID)(i)
        End If
        Dim LName As String = NameCut(result, LordNameMaxLen)
        AddToLog(LogID, "Selected lord name for race " & comm.defValues.RaceNumberToRaceChar(RaceID) & ": " & LName)
        Return LName
    End Function
    Private Function WeightArray(Optional ByVal UserWeightMultiplier As Double = 1, _
                                 Optional ByRef unitID As String = "") As Double()
        Dim weight(UBound(users)) As Double
        For w As Integer = 0 To UBound(users) Step 1
            If unitID = "" Then
                weight(w) = users(w).weight
            ElseIf users(w).unit.ToUpper = unitID.ToUpper Then
                weight(w) = users(w).increasedWeight
            Else
                weight(w) = users(w).decreasedWeight
            End If
        Next w
        For w As Integer = 0 To UBound(users) Step 1
            If Not w = NoneNameId AndAlso Not commonIDs.Contains(w) Then
                weight(NoneNameId) -= users(w).weight * NoneWeightMultiplier()
            End If
        Next w
        For w As Integer = 0 To UBound(users) Step 1
            If Not w = NoneNameId Then weight(w) *= UserWeightMultiplier
        Next w
        Return weight
    End Function
    Private Sub calcUWeigt(ByRef uCount As Dictionary(Of String, Integer()))
        Dim stacksCount As Integer = 0
        If Not IsNothing(uCount) Then
            For Each i As Integer() In uCount.Values
                stacksCount += i(0)
            Next i
        End If
        Dim minWeight As Double
        Dim desiredUnitCount As Integer
        For i As Integer = 0 To UBound(users) Step 1
            If Not i = NoneNameId AndAlso Not IsNothing(uCount) AndAlso uCount.ContainsKey(users(i).unit.ToUpper) Then
                desiredUnitCount = uCount.Item(users(i).unit.ToUpper)(0)
                minWeight = DesiredUnitMinWeightMultiplier * users(i).weight
                users(i).unitsCount = desiredUnitCount

                users(i).increasedWeight = DesiredUnitWeightMultiplier * users(i).weight
                users(i).decreasedWeight = (users(i).weight * stacksCount - users(i).increasedWeight * desiredUnitCount) / Math.Max(1, stacksCount - desiredUnitCount)
                If users(i).decreasedWeight < minWeight Then
                    users(i).decreasedWeight = minWeight
                    users(i).increasedWeight = (users(i).weight * stacksCount - (stacksCount - desiredUnitCount) * users(i).decreasedWeight) / desiredUnitCount
                End If
            Else
                users(i).increasedWeight = users(i).weight
                users(i).decreasedWeight = users(i).weight
            End If
        Next i
    End Sub
#End Region
#Region "Objects"
    Public Function CityName(ByRef ID As String, ByRef RaceID As Integer) As String
        If comm.defValues.capitalToGeneratorRace.ContainsKey(ID.ToUpper) Then
            Return customObjectsNames.GetCapitalName(comm.defValues.generatorRaceToCapitalID(comm.defValues.RaceNumberToRaceChar(RaceID)))
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyTown) Then
            Return customObjectsNames.GetCityName(ID.Remove(ID.Length - 1) & "@" & comm.defValues.RaceNumberToRaceChar(RaceID))
        Else
            Throw New Exception("Unknown object ID: " & ID)
        End If
    End Function
    Public Function ObjectName(ByRef ID As String) As String
        If ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyRuin) Then
            Return customObjectsNames.GetRuinName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMerchant) Then
            Return customObjectsNames.GetVendorName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMage) Then
            Return customObjectsNames.GetMageName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMercenaries) Then
            Return customObjectsNames.GetMercenaryName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyTrainer) Then
            Return customObjectsNames.GetTrainerName(ID)
        Else
            Throw New Exception("Unknown object ID: " & ID)
        End If
    End Function
    Public Function ObjectDescription(ByRef ID As String) As String
        Return customObjectsNames.GetDescription(ID)
    End Function
#End Region
#Region "Reset available names"
    Friend Sub ResetNames(ByRef newMapGen As Boolean, ByRef LogID As Integer)
        If newMapGen Then
            Call ClearLog(LogID)
            Call AddToLog(LogID, initializationLog)
            Call calcUWeigt(Nothing)
            Call ResetUsedStackNames(LogID)
            Call ResetLordsNames(LogID)
        End If
    End Sub
    Private Sub ResetUsedStackNames(ByRef LogID As Integer)
        If Not IsNothing(users) Then
            AddToLog(LogID, "Refreshing stack names list")
            commonIDs.Clear()
            For i As Integer = 0 To UBound(users) Step 1
                commonIDs.Add(i)
            Next i
        End If
    End Sub
    Private Sub ResetLordsNames(ByRef LogID As Integer)
        AddToLog(LogID, "Refreshing lords names list")

        If Not IsNothing(users) Then
            lordIDs.Clear()
            For i As Integer = 0 To UBound(users) Step 1
                If Not i = NoneNameId And users(i).weight > LordMinWeight Then lordIDs.Add(i)
            Next i
        End If

        defaultLords.Clear()
        For i As Integer = 0 To UBound(defaultLordNames) Step 1
            If Not IsNothing(defaultLordNames(i)) Then
                defaultLords.Add(i, New RandomSelection(defaultLordNames(i).Length, rndgen))
                For j As Integer = 0 To UBound(defaultLordNames(i)) Step 1
                    defaultLords.Item(i).Add(j)
                Next j
            End If
        Next i
    End Sub
#End Region
End Class
