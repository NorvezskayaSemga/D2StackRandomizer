Public Class SetName

    Private Const link As String = "https://norvezskayasemga.wixsite.com/d2bfwmod/donaters"
    Private Const responseHeader As String = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.140 Safari/537.36 Edge/17.17134"
    Private Const path As String = ".\Donaters.txt"
    Private Const lordDonationThreshold As Double = 999

    Friend Const StackNameMaxLen As Integer = 30
    Friend Const LordNameMaxLen As Integer = 15
    Friend Const CityNameMaxLen As Integer = 20
    Friend Const CapitalNameMaxLen As Integer = 20
    Friend Const MerchantNameMaxLen As Integer = 35
    Friend Const RuinsNameMaxLen As Integer = 25
    Friend Const DescriptionMaxLen As Integer = 100

    Private users() As Donater = Nothing
    Private comm As Common
    Private rndgen As New RndValueGen
    Private exclude As New List(Of String)
    Private excludeRace As New List(Of Integer)
    Private commonIDs, lordIDs As RandomSelection
    Private defaultLords As New Dictionary(Of Integer, RandomSelection)
    Private LordMinWeight As Double
    Private defaultLordNames(14)() As String
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
        Call Me.New(lang, modName, Nothing)
    End Sub
    Public Sub New(ByVal lang As GenDefaultValues.TextLanguage, ByVal modName As String, ByRef gm As NevendaarTools.GameModel)
        comm = New Common(modName)
        log = New Log(comm)

        customObjectsNames = New GenDefaultValues.MapObjectsText(lang, comm.defValues, gm)

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

        If Not IsNothing(users) Then
            For i As Integer = 0 To UBound(users) Step 1
                If IsNothing(users(i).unit) OrElse users(i).unit = "" Then users(i).unit = GenDefaultValues.emptyItem.ToUpper
                If Not lang = GenDefaultValues.TextLanguage.Rus AndAlso Transliterator.HasCyrilicCharacters(users(i).name) Then
                    users(i).name = Transliterator.CyrillicToLatinRussian(users(i).name)
                End If
            Next i
            ReDim Preserve users(users.Length)
        Else
            ReDim users(0)
        End If
        NoneNameId = UBound(users)
        users(NoneNameId) = New Donater With {.name = "!!!NameBug!!!", .unit = "", .weight = 0}
        For i As Integer = 0 To UBound(users) - 1 Step 1
            users(NoneNameId).weight += users(i).weight
        Next i
        If users(NoneNameId).weight = 0 Then users(NoneNameId).weight = 1
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
            w.Headers.Add(System.Net.HttpRequestHeader.UserAgent, responseHeader)

            Dim byteData() As Byte = w.DownloadData(link)
            Dim d As String = Text.Encoding.UTF8.GetString(byteData)

            Dim tableKeyword As String = "#this_is_line_with_donaters_table#"
            Dim tableLineStart As String = "<script type=""application/json"""

            Dim str() As String = d.Replace(Chr(13), Chr(10)).Split(Chr(10))
            Dim base As String = ""
            For s As Integer = 0 To UBound(str) Step 1
                If str(s).Trim.StartsWith(tableLineStart) AndAlso str(s).Contains(tableKeyword) Then
                    base = str(s)
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
                If wsum = 0 Then
                    users = Nothing
                Else
                    For i As Integer = 0 To UBound(users) Step 1
                        users(i).weight /= wsum
                    Next i
                    LordMinWeight = lordDonationThreshold / wsum
                End If
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
        Try
            If Not unit.unitID.ToUpper = users(userID).unit.ToUpper Then
                If excludeRace.Contains(unit.race) Then Return True
                If exclude.Contains(unit.unitID) Then Return True
            End If
            Return False
        Catch ex As Exception
            Dim u As String
            If Not IsNothing(users) AndAlso (userID > -1 And userID < users.Length) AndAlso Not IsNothing(users(userID).unit) Then
                u = users(userID).unit
            Else
                u = "nothing"
            End If
            Throw New Exception(ex.Message & _
                                vbNewLine & "userID: " & userID & " unit = " & u &
                                vbNewLine & "users.Length: " & users.Length &
                                vbNewLine & "excludeRace is nothing: " & IsNothing(excludeRace) &
                                vbNewLine & "exclude is nothing: " & IsNothing(exclude))
        End Try
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
                weight(NoneNameId) = Math.Max(weight(NoneNameId) - users(w).weight * NoneWeightMultiplier(), 0)
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

Public Class Transliterator

    Private Structure CustomStringBuilder
        Implements IDisposable

        Private _array As Char()
        Private _index As Integer

        Public Sub New(ByVal capacity As Integer)
            _array = New Char(capacity - 1) {}
            _index = 0
        End Sub

        Public Sub Append(ByVal c As Char)
            _array(Math.Min(System.Threading.Interlocked.Increment(_index), _index - 1)) = c
        End Sub

        Public Overrides Function ToString() As String
            Return New String(_array, 0, _index)
        End Function

        Sub Dispose() Implements System.IDisposable.Dispose
            _index = 0
            _array = Nothing
        End Sub
    End Structure

    Private Const _Cyr As String = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
    Public Shared Function HasCyrilicCharacters(ByVal txt As String) As Boolean
        For Each v As Char In txt
            If _Cyr.Contains(v.ToString.ToLower) Then Return True
        Next v
        Return False
    End Function

    Public Shared Function CyrillicToLatinRussian(ByVal text As String) As String
        Dim sb As CustomStringBuilder = New CustomStringBuilder(text.Length * 3)
        Using sb
            Dim state = 0

            For i = 0 To text.Length - 1
                Dim c = text(i)

                Select Case state
                    Case 0

                        Select Case c
                            Case "Ё"c
                                sb.Append("Y"c)
                                sb.Append("o"c)
                            Case "А"c
                                sb.Append("A"c)
                            Case "Б"c
                                sb.Append("B"c)
                            Case "В"c
                                sb.Append("V"c)
                            Case "Г"c
                                sb.Append("G"c)
                            Case "Д"c
                                sb.Append("D"c)
                            Case "Е"c
                                sb.Append("E"c)
                            Case "Ж"c
                                sb.Append("Z"c)
                                sb.Append("h"c)
                            Case "З"c
                                sb.Append("Z"c)
                            Case "И"c
                                sb.Append("I"c)
                            Case "Й"c
                                sb.Append("J"c)
                            Case "К"c
                                sb.Append("K"c)
                            Case "Л"c
                                sb.Append("L"c)
                            Case "М"c
                                sb.Append("M"c)
                            Case "Н"c
                                sb.Append("N"c)
                            Case "О"c
                                sb.Append("O"c)
                            Case "П"c
                                sb.Append("P"c)
                            Case "Р"c
                                sb.Append("R"c)
                            Case "С"c
                                sb.Append("S"c)
                            Case "Т"c
                                sb.Append("T"c)
                            Case "У"c
                                sb.Append("U"c)
                            Case "Ф"c
                                sb.Append("F"c)
                            Case "Х"c
                                sb.Append("X"c)
                            Case "Ц"c
                                state = 2
                            Case "Ч"c
                                sb.Append("C"c)
                                sb.Append("h"c)
                            Case "Ш"c
                                sb.Append("S"c)
                                sb.Append("h"c)
                            Case "Щ"c
                                sb.Append("S"c)
                                sb.Append("h"c)
                                sb.Append("h"c)
                            Case "Ъ"c
                                sb.Append("`"c)
                                sb.Append("`"c)
                            Case "Ы"c
                                sb.Append("Y"c)
                                sb.Append("`"c)
                            Case "Ь"c
                                sb.Append("`"c)
                            Case "Э"c
                                sb.Append("E"c)
                                sb.Append("`"c)
                            Case "Ю"c
                                sb.Append("Y"c)
                                sb.Append("u"c)
                            Case "Я"c
                                sb.Append("Y"c)
                                sb.Append("a"c)
                            Case "а"c
                                sb.Append("a"c)
                            Case "б"c
                                sb.Append("b"c)
                            Case "в"c
                                sb.Append("v"c)
                            Case "г"c
                                sb.Append("g"c)
                            Case "д"c
                                sb.Append("d"c)
                            Case "е"c
                                sb.Append("e"c)
                            Case "ж"c
                                sb.Append("z"c)
                                sb.Append("h"c)
                            Case "з"c
                                sb.Append("z"c)
                            Case "и"c
                                sb.Append("i"c)
                            Case "й"c
                                sb.Append("j"c)
                            Case "к"c
                                sb.Append("k"c)
                            Case "л"c
                                sb.Append("l"c)
                            Case "м"c
                                sb.Append("m"c)
                            Case "н"c
                                sb.Append("n"c)
                            Case "о"c
                                sb.Append("o"c)
                            Case "п"c
                                sb.Append("p"c)
                            Case "р"c
                                sb.Append("r"c)
                            Case "с"c
                                sb.Append("s"c)
                            Case "т"c
                                sb.Append("t"c)
                            Case "у"c
                                sb.Append("u"c)
                            Case "ф"c
                                sb.Append("f"c)
                            Case "х"c
                                sb.Append("x"c)
                            Case "ц"c
                                state = 1
                            Case "ч"c
                                sb.Append("c"c)
                                sb.Append("h"c)
                            Case "ш"c
                                sb.Append("s"c)
                                sb.Append("h"c)
                            Case "щ"c
                                sb.Append("s"c)
                                sb.Append("h"c)
                                sb.Append("h"c)
                            Case "ъ"c
                                sb.Append("`"c)
                                sb.Append("`"c)
                            Case "ы"c
                                sb.Append("y"c)
                                sb.Append("`"c)
                            Case "ь"c
                                sb.Append("`"c)
                            Case "э"c
                                sb.Append("e"c)
                                sb.Append("`"c)
                            Case "ю"c
                                sb.Append("y"c)
                                sb.Append("u"c)
                            Case "я"c
                                sb.Append("y"c)
                                sb.Append("a"c)
                            Case "ё"c
                                sb.Append("y"c)
                                sb.Append("o"c)
                            Case "Ѣ"c
                                sb.Append("Y"c)
                                sb.Append("e"c)
                            Case "ѣ"c
                                sb.Append("y"c)
                                sb.Append("e"c)
                            Case "Ѳ"c
                                sb.Append("F"c)
                                sb.Append("h"c)
                            Case "ѳ"c
                                sb.Append("f"c)
                                sb.Append("h"c)
                            Case "Ѵ"c
                                sb.Append("Y"c)
                                sb.Append("h"c)
                            Case "ѵ"c
                                sb.Append("y"c)
                                sb.Append("h"c)
                            Case "’"c
                                sb.Append("'"c)
                            Case "№"c
                                sb.Append("#"c)
                            Case Else
                                sb.Append(c)
                        End Select

                    Case 1

                        Select Case c
                            Case "Ё"c
                                sb.Append("c"c)
                                sb.Append("Y"c)
                                sb.Append("o"c)
                                state = 0
                            Case "А"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("A"c)
                                state = 0
                            Case "Б"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("B"c)
                                state = 0
                            Case "В"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("V"c)
                                state = 0
                            Case "Г"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("G"c)
                                state = 0
                            Case "Д"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("D"c)
                                state = 0
                            Case "Е"c
                                sb.Append("c"c)
                                sb.Append("E"c)
                                state = 0
                            Case "Ж"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("Z"c)
                                sb.Append("h"c)
                                state = 0
                            Case "З"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("Z"c)
                                state = 0
                            Case "И"c
                                sb.Append("c"c)
                                sb.Append("I"c)
                                state = 0
                            Case "Й"c
                                sb.Append("c"c)
                                sb.Append("J"c)
                                state = 0
                            Case "К"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("K"c)
                                state = 0
                            Case "Л"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("L"c)
                                state = 0
                            Case "М"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("M"c)
                                state = 0
                            Case "Н"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("N"c)
                                state = 0
                            Case "О"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("O"c)
                                state = 0
                            Case "П"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("P"c)
                                state = 0
                            Case "Р"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("R"c)
                                state = 0
                            Case "С"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("S"c)
                                state = 0
                            Case "Т"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("T"c)
                                state = 0
                            Case "У"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("U"c)
                                state = 0
                            Case "Ф"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("F"c)
                                state = 0
                            Case "Х"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("X"c)
                                state = 0
                            Case "Ц"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                state = 2
                            Case "Ч"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("C"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Ш"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("S"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Щ"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("S"c)
                                sb.Append("h"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Ъ"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Ы"c
                                sb.Append("c"c)
                                sb.Append("Y"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Ь"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Э"c
                                sb.Append("c"c)
                                sb.Append("E"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Ю"c
                                sb.Append("c"c)
                                sb.Append("Y"c)
                                sb.Append("u"c)
                                state = 0
                            Case "Я"c
                                sb.Append("c"c)
                                sb.Append("Y"c)
                                sb.Append("a"c)
                                state = 0
                            Case "а"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("a"c)
                                state = 0
                            Case "б"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("b"c)
                                state = 0
                            Case "в"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("v"c)
                                state = 0
                            Case "г"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("g"c)
                                state = 0
                            Case "д"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("d"c)
                                state = 0
                            Case "е"c
                                sb.Append("c"c)
                                sb.Append("e"c)
                                state = 0
                            Case "ж"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("z"c)
                                sb.Append("h"c)
                                state = 0
                            Case "з"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("z"c)
                                state = 0
                            Case "и"c
                                sb.Append("c"c)
                                sb.Append("i"c)
                                state = 0
                            Case "й"c
                                sb.Append("c"c)
                                sb.Append("j"c)
                                state = 0
                            Case "к"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("k"c)
                                state = 0
                            Case "л"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("l"c)
                                state = 0
                            Case "м"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("m"c)
                                state = 0
                            Case "н"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("n"c)
                                state = 0
                            Case "о"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("o"c)
                                state = 0
                            Case "п"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("p"c)
                                state = 0
                            Case "р"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("r"c)
                                state = 0
                            Case "с"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("s"c)
                                state = 0
                            Case "т"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("t"c)
                                state = 0
                            Case "у"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("u"c)
                                state = 0
                            Case "ф"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("f"c)
                                state = 0
                            Case "х"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("x"c)
                                state = 0
                            Case "ц"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                            Case "ч"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("c"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ш"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("s"c)
                                sb.Append("h"c)
                                state = 0
                            Case "щ"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("s"c)
                                sb.Append("h"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ъ"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                sb.Append("`"c)
                                state = 0
                            Case "ы"c
                                sb.Append("c"c)
                                sb.Append("y"c)
                                sb.Append("`"c)
                                state = 0
                            Case "ь"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                state = 0
                            Case "э"c
                                sb.Append("c"c)
                                sb.Append("e"c)
                                sb.Append("`"c)
                                state = 0
                            Case "ю"c
                                sb.Append("c"c)
                                sb.Append("y"c)
                                sb.Append("u"c)
                                state = 0
                            Case "я"c
                                sb.Append("c"c)
                                sb.Append("y"c)
                                sb.Append("a"c)
                                state = 0
                            Case "ё"c
                                sb.Append("c"c)
                                sb.Append("y"c)
                                sb.Append("o"c)
                                state = 0
                            Case "Ѣ"c
                                sb.Append("c"c)
                                sb.Append("Y"c)
                                sb.Append("e"c)
                                state = 0
                            Case "ѣ"c
                                sb.Append("c"c)
                                sb.Append("y"c)
                                sb.Append("e"c)
                                state = 0
                            Case "Ѳ"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("F"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ѳ"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("f"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Ѵ"c
                                sb.Append("c"c)
                                sb.Append("Y"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ѵ"c
                                sb.Append("c"c)
                                sb.Append("y"c)
                                sb.Append("h"c)
                                state = 0
                            Case "’"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("'"c)
                                state = 0
                            Case "№"c
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append("#"c)
                                state = 0
                            Case Else
                                sb.Append("c"c)
                                sb.Append("z"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 2

                        Select Case c
                            Case "Ё"c
                                sb.Append("C"c)
                                sb.Append("Y"c)
                                sb.Append("o"c)
                                state = 0
                            Case "А"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("A"c)
                                state = 0
                            Case "Б"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("B"c)
                                state = 0
                            Case "В"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("V"c)
                                state = 0
                            Case "Г"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("G"c)
                                state = 0
                            Case "Д"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("D"c)
                                state = 0
                            Case "Е"c
                                sb.Append("C"c)
                                sb.Append("E"c)
                                state = 0
                            Case "Ж"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("Z"c)
                                sb.Append("h"c)
                                state = 0
                            Case "З"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("Z"c)
                                state = 0
                            Case "И"c
                                sb.Append("C"c)
                                sb.Append("I"c)
                                state = 0
                            Case "Й"c
                                sb.Append("C"c)
                                sb.Append("J"c)
                                state = 0
                            Case "К"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("K"c)
                                state = 0
                            Case "Л"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("L"c)
                                state = 0
                            Case "М"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("M"c)
                                state = 0
                            Case "Н"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("N"c)
                                state = 0
                            Case "О"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("O"c)
                                state = 0
                            Case "П"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("P"c)
                                state = 0
                            Case "Р"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("R"c)
                                state = 0
                            Case "С"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("S"c)
                                state = 0
                            Case "Т"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("T"c)
                                state = 0
                            Case "У"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("U"c)
                                state = 0
                            Case "Ф"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("F"c)
                                state = 0
                            Case "Х"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("X"c)
                                state = 0
                            Case "Ц"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                            Case "Ч"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("C"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Ш"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("S"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Щ"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("S"c)
                                sb.Append("h"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Ъ"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Ы"c
                                sb.Append("C"c)
                                sb.Append("Y"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Ь"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Э"c
                                sb.Append("C"c)
                                sb.Append("E"c)
                                sb.Append("`"c)
                                state = 0
                            Case "Ю"c
                                sb.Append("C"c)
                                sb.Append("Y"c)
                                sb.Append("u"c)
                                state = 0
                            Case "Я"c
                                sb.Append("C"c)
                                sb.Append("Y"c)
                                sb.Append("a"c)
                                state = 0
                            Case "а"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("a"c)
                                state = 0
                            Case "б"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("b"c)
                                state = 0
                            Case "в"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("v"c)
                                state = 0
                            Case "г"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("g"c)
                                state = 0
                            Case "д"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("d"c)
                                state = 0
                            Case "е"c
                                sb.Append("C"c)
                                sb.Append("e"c)
                                state = 0
                            Case "ж"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("z"c)
                                sb.Append("h"c)
                                state = 0
                            Case "з"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("z"c)
                                state = 0
                            Case "и"c
                                sb.Append("C"c)
                                sb.Append("i"c)
                                state = 0
                            Case "й"c
                                sb.Append("C"c)
                                sb.Append("j"c)
                                state = 0
                            Case "к"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("k"c)
                                state = 0
                            Case "л"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("l"c)
                                state = 0
                            Case "м"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("m"c)
                                state = 0
                            Case "н"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("n"c)
                                state = 0
                            Case "о"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("o"c)
                                state = 0
                            Case "п"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("p"c)
                                state = 0
                            Case "р"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("r"c)
                                state = 0
                            Case "с"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("s"c)
                                state = 0
                            Case "т"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("t"c)
                                state = 0
                            Case "у"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("u"c)
                                state = 0
                            Case "ф"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("f"c)
                                state = 0
                            Case "х"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("x"c)
                                state = 0
                            Case "ц"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                state = 1
                            Case "ч"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("c"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ш"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("s"c)
                                sb.Append("h"c)
                                state = 0
                            Case "щ"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("s"c)
                                sb.Append("h"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ъ"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                sb.Append("`"c)
                                state = 0
                            Case "ы"c
                                sb.Append("C"c)
                                sb.Append("y"c)
                                sb.Append("`"c)
                                state = 0
                            Case "ь"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("`"c)
                                state = 0
                            Case "э"c
                                sb.Append("C"c)
                                sb.Append("e"c)
                                sb.Append("`"c)
                                state = 0
                            Case "ю"c
                                sb.Append("C"c)
                                sb.Append("y"c)
                                sb.Append("u"c)
                                state = 0
                            Case "я"c
                                sb.Append("C"c)
                                sb.Append("y"c)
                                sb.Append("a"c)
                                state = 0
                            Case "ё"c
                                sb.Append("C"c)
                                sb.Append("y"c)
                                sb.Append("o"c)
                                state = 0
                            Case "Ѣ"c
                                sb.Append("C"c)
                                sb.Append("Y"c)
                                sb.Append("e"c)
                                state = 0
                            Case "ѣ"c
                                sb.Append("C"c)
                                sb.Append("y"c)
                                sb.Append("e"c)
                                state = 0
                            Case "Ѳ"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("F"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ѳ"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("f"c)
                                sb.Append("h"c)
                                state = 0
                            Case "Ѵ"c
                                sb.Append("C"c)
                                sb.Append("Y"c)
                                sb.Append("h"c)
                                state = 0
                            Case "ѵ"c
                                sb.Append("C"c)
                                sb.Append("y"c)
                                sb.Append("h"c)
                                state = 0
                            Case "’"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("'"c)
                                state = 0
                            Case "№"c
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append("#"c)
                                state = 0
                            Case Else
                                sb.Append("C"c)
                                sb.Append("z"c)
                                sb.Append(c)
                                state = 0
                        End Select
                End Select
            Next

            Select Case state
                Case 1
                    sb.Append("c"c)
                    sb.Append("z"c)
                Case 2
                    sb.Append("C"c)
                    sb.Append("z"c)
            End Select

            Return sb.ToString()
        End Using
    End Function

    Public Shared Function LatinToCyrillicRussian(ByVal text As String) As String
        Dim sb As CustomStringBuilder = New CustomStringBuilder(text.Length)
        Using sb
            Dim state = 0

            For i = 0 To text.Length - 1
                Dim c = text(i)

                Select Case state
                    Case 0

                        Select Case c
                            Case "#"c
                                sb.Append("№"c)
                            Case "'"c
                                sb.Append("’"c)
                            Case "A"c
                                sb.Append("А"c)
                            Case "B"c
                                sb.Append("Б"c)
                            Case "C"c
                                state = 3
                            Case "D"c
                                sb.Append("Д"c)
                            Case "E"c
                                state = 5
                            Case "F"c
                                state = 7
                            Case "G"c
                                sb.Append("Г"c)
                            Case "I"c
                                sb.Append("И"c)
                            Case "J"c
                                sb.Append("Й"c)
                            Case "K"c
                                sb.Append("К"c)
                            Case "L"c
                                sb.Append("Л"c)
                            Case "M"c
                                sb.Append("М"c)
                            Case "N"c
                                sb.Append("Н"c)
                            Case "O"c
                                sb.Append("О"c)
                            Case "P"c
                                sb.Append("П"c)
                            Case "R"c
                                sb.Append("Р"c)
                            Case "S"c
                                state = 9
                            Case "T"c
                                sb.Append("Т"c)
                            Case "U"c
                                sb.Append("У"c)
                            Case "V"c
                                sb.Append("В"c)
                            Case "X"c
                                sb.Append("Х"c)
                            Case "Y"c
                                state = 11
                            Case "Z"c
                                state = 13
                            Case "`"c
                                state = 1
                            Case "a"c
                                sb.Append("а"c)
                            Case "b"c
                                sb.Append("б"c)
                            Case "c"c
                                state = 2
                            Case "d"c
                                sb.Append("д"c)
                            Case "e"c
                                state = 4
                            Case "f"c
                                state = 6
                            Case "g"c
                                sb.Append("г"c)
                            Case "i"c
                                sb.Append("и"c)
                            Case "j"c
                                sb.Append("й"c)
                            Case "k"c
                                sb.Append("к"c)
                            Case "l"c
                                sb.Append("л"c)
                            Case "m"c
                                sb.Append("м"c)
                            Case "n"c
                                sb.Append("н"c)
                            Case "o"c
                                sb.Append("о"c)
                            Case "p"c
                                sb.Append("п"c)
                            Case "r"c
                                sb.Append("р"c)
                            Case "s"c
                                state = 8
                            Case "t"c
                                sb.Append("т"c)
                            Case "u"c
                                sb.Append("у"c)
                            Case "v"c
                                sb.Append("в"c)
                            Case "x"c
                                sb.Append("х"c)
                            Case "y"c
                                state = 10
                            Case "z"c
                                state = 12
                            Case Else
                                sb.Append(c)
                        End Select

                    Case 1

                        Select Case c
                            Case "#"c
                                sb.Append("ь"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("ь"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("ь"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("ь"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("ь"c)
                                state = 3
                            Case "D"c
                                sb.Append("ь"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("ь"c)
                                state = 5
                            Case "F"c
                                sb.Append("ь"c)
                                state = 7
                            Case "G"c
                                sb.Append("ь"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("ь"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("ь"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("ь"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("ь"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("ь"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("ь"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("ь"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("ь"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("ь"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("ь"c)
                                state = 9
                            Case "T"c
                                sb.Append("ь"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("ь"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("ь"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("ь"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("ь"c)
                                state = 11
                            Case "Z"c
                                sb.Append("ь"c)
                                state = 13
                            Case "`"c
                                sb.Append("ъ"c)
                                state = 0
                            Case "a"c
                                sb.Append("ь"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("ь"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("ь"c)
                                state = 2
                            Case "d"c
                                sb.Append("ь"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("ь"c)
                                state = 4
                            Case "f"c
                                sb.Append("ь"c)
                                state = 6
                            Case "g"c
                                sb.Append("ь"c)
                                sb.Append("г"c)
                                state = 0
                            Case "i"c
                                sb.Append("ь"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("ь"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("ь"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("ь"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("ь"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("ь"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("ь"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("ь"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("ь"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("ь"c)
                                state = 8
                            Case "t"c
                                sb.Append("ь"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("ь"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("ь"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("ь"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("ь"c)
                                state = 10
                            Case "z"c
                                sb.Append("ь"c)
                                state = 12
                            Case Else
                                sb.Append("ь"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 2

                        Select Case c
                            Case "#"c
                                sb.Append("ц"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("ц"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("ц"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("ц"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("ц"c)
                                state = 3
                            Case "D"c
                                sb.Append("ц"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("ц"c)
                                state = 5
                            Case "F"c
                                sb.Append("ц"c)
                                state = 7
                            Case "G"c
                                sb.Append("ц"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("ц"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("ц"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("ц"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("ц"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("ц"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("ц"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("ц"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("ц"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("ц"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("ц"c)
                                state = 9
                            Case "T"c
                                sb.Append("ц"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("ц"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("ц"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("ц"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("ц"c)
                                state = 11
                            Case "Z"c
                                sb.Append("ц"c)
                                state = 13
                            Case "`"c
                                sb.Append("ц"c)
                                state = 1
                            Case "a"c
                                sb.Append("ц"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("ц"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("ц"c)
                            Case "d"c
                                sb.Append("ц"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("ц"c)
                                state = 4
                            Case "f"c
                                sb.Append("ц"c)
                                state = 6
                            Case "g"c
                                sb.Append("ц"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("ч"c)
                                state = 0
                            Case "i"c
                                sb.Append("ц"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("ц"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("ц"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("ц"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("ц"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("ц"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("ц"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("ц"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("ц"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("ц"c)
                                state = 8
                            Case "t"c
                                sb.Append("ц"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("ц"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("ц"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("ц"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("ц"c)
                                state = 10
                            Case "z"c
                                sb.Append("ц"c)
                                state = 0
                            Case Else
                                sb.Append("ц"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 3

                        Select Case c
                            Case "#"c
                                sb.Append("Ц"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("Ц"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("Ц"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("Ц"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("Ц"c)
                            Case "D"c
                                sb.Append("Ц"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("Ц"c)
                                state = 5
                            Case "F"c
                                sb.Append("Ц"c)
                                state = 7
                            Case "G"c
                                sb.Append("Ц"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("Ц"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("Ц"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("Ц"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("Ц"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("Ц"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("Ц"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("Ц"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("Ц"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("Ц"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("Ц"c)
                                state = 9
                            Case "T"c
                                sb.Append("Ц"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("Ц"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("Ц"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("Ц"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("Ц"c)
                                state = 11
                            Case "Z"c
                                sb.Append("Ц"c)
                                state = 13
                            Case "`"c
                                sb.Append("Ц"c)
                                state = 1
                            Case "a"c
                                sb.Append("Ц"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("Ц"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("Ц"c)
                                state = 2
                            Case "d"c
                                sb.Append("Ц"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("Ц"c)
                                state = 4
                            Case "f"c
                                sb.Append("Ц"c)
                                state = 6
                            Case "g"c
                                sb.Append("Ц"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("Ч"c)
                                state = 0
                            Case "i"c
                                sb.Append("Ц"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("Ц"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("Ц"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("Ц"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("Ц"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("Ц"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("Ц"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("Ц"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("Ц"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("Ц"c)
                                state = 8
                            Case "t"c
                                sb.Append("Ц"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("Ц"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("Ц"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("Ц"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("Ц"c)
                                state = 10
                            Case "z"c
                                sb.Append("Ц"c)
                                state = 0
                            Case Else
                                sb.Append("Ц"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 4

                        Select Case c
                            Case "#"c
                                sb.Append("е"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("е"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("е"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("е"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("е"c)
                                state = 3
                            Case "D"c
                                sb.Append("е"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("е"c)
                                state = 5
                            Case "F"c
                                sb.Append("е"c)
                                state = 7
                            Case "G"c
                                sb.Append("е"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("е"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("е"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("е"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("е"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("е"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("е"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("е"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("е"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("е"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("е"c)
                                state = 9
                            Case "T"c
                                sb.Append("е"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("е"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("е"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("е"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("е"c)
                                state = 11
                            Case "Z"c
                                sb.Append("е"c)
                                state = 13
                            Case "`"c
                                sb.Append("э"c)
                                state = 0
                            Case "a"c
                                sb.Append("е"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("е"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("е"c)
                                state = 2
                            Case "d"c
                                sb.Append("е"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("е"c)
                            Case "f"c
                                sb.Append("е"c)
                                state = 6
                            Case "g"c
                                sb.Append("е"c)
                                sb.Append("г"c)
                                state = 0
                            Case "i"c
                                sb.Append("е"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("е"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("е"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("е"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("е"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("е"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("е"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("е"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("е"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("е"c)
                                state = 8
                            Case "t"c
                                sb.Append("е"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("е"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("е"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("е"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("е"c)
                                state = 10
                            Case "z"c
                                sb.Append("е"c)
                                state = 12
                            Case Else
                                sb.Append("е"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 5

                        Select Case c
                            Case "#"c
                                sb.Append("Е"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("Е"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("Е"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("Е"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("Е"c)
                                state = 3
                            Case "D"c
                                sb.Append("Е"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("Е"c)
                            Case "F"c
                                sb.Append("Е"c)
                                state = 7
                            Case "G"c
                                sb.Append("Е"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("Е"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("Е"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("Е"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("Е"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("Е"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("Е"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("Е"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("Е"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("Е"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("Е"c)
                                state = 9
                            Case "T"c
                                sb.Append("Е"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("Е"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("Е"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("Е"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("Е"c)
                                state = 11
                            Case "Z"c
                                sb.Append("Е"c)
                                state = 13
                            Case "`"c
                                sb.Append("Э"c)
                                state = 0
                            Case "a"c
                                sb.Append("Е"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("Е"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("Е"c)
                                state = 2
                            Case "d"c
                                sb.Append("Е"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("Е"c)
                                state = 4
                            Case "f"c
                                sb.Append("Е"c)
                                state = 6
                            Case "g"c
                                sb.Append("Е"c)
                                sb.Append("г"c)
                                state = 0
                            Case "i"c
                                sb.Append("Е"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("Е"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("Е"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("Е"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("Е"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("Е"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("Е"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("Е"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("Е"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("Е"c)
                                state = 8
                            Case "t"c
                                sb.Append("Е"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("Е"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("Е"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("Е"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("Е"c)
                                state = 10
                            Case "z"c
                                sb.Append("Е"c)
                                state = 12
                            Case Else
                                sb.Append("Е"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 6

                        Select Case c
                            Case "#"c
                                sb.Append("ф"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("ф"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("ф"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("ф"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("ф"c)
                                state = 3
                            Case "D"c
                                sb.Append("ф"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("ф"c)
                                state = 5
                            Case "F"c
                                sb.Append("ф"c)
                                state = 7
                            Case "G"c
                                sb.Append("ф"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("ф"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("ф"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("ф"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("ф"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("ф"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("ф"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("ф"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("ф"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("ф"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("ф"c)
                                state = 9
                            Case "T"c
                                sb.Append("ф"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("ф"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("ф"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("ф"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("ф"c)
                                state = 11
                            Case "Z"c
                                sb.Append("ф"c)
                                state = 13
                            Case "`"c
                                sb.Append("ф"c)
                                state = 1
                            Case "a"c
                                sb.Append("ф"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("ф"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("ф"c)
                                state = 2
                            Case "d"c
                                sb.Append("ф"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("ф"c)
                                state = 4
                            Case "f"c
                                sb.Append("ф"c)
                            Case "g"c
                                sb.Append("ф"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("ѳ"c)
                                state = 0
                            Case "i"c
                                sb.Append("ф"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("ф"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("ф"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("ф"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("ф"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("ф"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("ф"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("ф"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("ф"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("ф"c)
                                state = 8
                            Case "t"c
                                sb.Append("ф"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("ф"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("ф"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("ф"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("ф"c)
                                state = 10
                            Case "z"c
                                sb.Append("ф"c)
                                state = 12
                            Case Else
                                sb.Append("ф"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 7

                        Select Case c
                            Case "#"c
                                sb.Append("Ф"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("Ф"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("Ф"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("Ф"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("Ф"c)
                                state = 3
                            Case "D"c
                                sb.Append("Ф"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("Ф"c)
                                state = 5
                            Case "F"c
                                sb.Append("Ф"c)
                            Case "G"c
                                sb.Append("Ф"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("Ф"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("Ф"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("Ф"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("Ф"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("Ф"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("Ф"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("Ф"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("Ф"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("Ф"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("Ф"c)
                                state = 9
                            Case "T"c
                                sb.Append("Ф"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("Ф"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("Ф"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("Ф"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("Ф"c)
                                state = 11
                            Case "Z"c
                                sb.Append("Ф"c)
                                state = 13
                            Case "`"c
                                sb.Append("Ф"c)
                                state = 1
                            Case "a"c
                                sb.Append("Ф"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("Ф"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("Ф"c)
                                state = 2
                            Case "d"c
                                sb.Append("Ф"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("Ф"c)
                                state = 4
                            Case "f"c
                                sb.Append("Ф"c)
                                state = 6
                            Case "g"c
                                sb.Append("Ф"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("Ѳ"c)
                                state = 0
                            Case "i"c
                                sb.Append("Ф"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("Ф"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("Ф"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("Ф"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("Ф"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("Ф"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("Ф"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("Ф"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("Ф"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("Ф"c)
                                state = 8
                            Case "t"c
                                sb.Append("Ф"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("Ф"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("Ф"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("Ф"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("Ф"c)
                                state = 10
                            Case "z"c
                                sb.Append("Ф"c)
                                state = 12
                            Case Else
                                sb.Append("Ф"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 8

                        Select Case c
                            Case "#"c
                                sb.Append("с"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("с"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("с"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("с"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("с"c)
                                state = 3
                            Case "D"c
                                sb.Append("с"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("с"c)
                                state = 5
                            Case "F"c
                                sb.Append("с"c)
                                state = 7
                            Case "G"c
                                sb.Append("с"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("с"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("с"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("с"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("с"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("с"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("с"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("с"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("с"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("с"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("с"c)
                                state = 9
                            Case "T"c
                                sb.Append("с"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("с"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("с"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("с"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("с"c)
                                state = 11
                            Case "Z"c
                                sb.Append("с"c)
                                state = 13
                            Case "`"c
                                sb.Append("с"c)
                                state = 1
                            Case "a"c
                                sb.Append("с"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("с"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("с"c)
                                state = 2
                            Case "d"c
                                sb.Append("с"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("с"c)
                                state = 4
                            Case "f"c
                                sb.Append("с"c)
                                state = 6
                            Case "g"c
                                sb.Append("с"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                state = 14
                            Case "i"c
                                sb.Append("с"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("с"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("с"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("с"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("с"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("с"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("с"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("с"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("с"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("с"c)
                            Case "t"c
                                sb.Append("с"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("с"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("с"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("с"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("с"c)
                                state = 10
                            Case "z"c
                                sb.Append("с"c)
                                state = 12
                            Case Else
                                sb.Append("с"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 9

                        Select Case c
                            Case "#"c
                                sb.Append("С"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("С"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("С"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("С"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("С"c)
                                state = 3
                            Case "D"c
                                sb.Append("С"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("С"c)
                                state = 5
                            Case "F"c
                                sb.Append("С"c)
                                state = 7
                            Case "G"c
                                sb.Append("С"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("С"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("С"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("С"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("С"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("С"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("С"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("С"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("С"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("С"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("С"c)
                            Case "T"c
                                sb.Append("С"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("С"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("С"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("С"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("С"c)
                                state = 11
                            Case "Z"c
                                sb.Append("С"c)
                                state = 13
                            Case "`"c
                                sb.Append("С"c)
                                state = 1
                            Case "a"c
                                sb.Append("С"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("С"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("С"c)
                                state = 2
                            Case "d"c
                                sb.Append("С"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("С"c)
                                state = 4
                            Case "f"c
                                sb.Append("С"c)
                                state = 6
                            Case "g"c
                                sb.Append("С"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                state = 15
                            Case "i"c
                                sb.Append("С"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("С"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("С"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("С"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("С"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("С"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("С"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("С"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("С"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("С"c)
                                state = 8
                            Case "t"c
                                sb.Append("С"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("С"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("С"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("С"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("С"c)
                                state = 10
                            Case "z"c
                                sb.Append("С"c)
                                state = 12
                            Case Else
                                sb.Append("С"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 10

                        Select Case c
                            Case "#"c
                                sb.Append("y"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("y"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("y"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("y"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("y"c)
                                state = 3
                            Case "D"c
                                sb.Append("y"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("y"c)
                                state = 5
                            Case "F"c
                                sb.Append("y"c)
                                state = 7
                            Case "G"c
                                sb.Append("y"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("y"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("y"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("y"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("y"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("y"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("y"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("y"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("y"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("y"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("y"c)
                                state = 9
                            Case "T"c
                                sb.Append("y"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("y"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("y"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("y"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("y"c)
                                state = 11
                            Case "Z"c
                                sb.Append("y"c)
                                state = 13
                            Case "`"c
                                sb.Append("ы"c)
                                state = 0
                            Case "a"c
                                sb.Append("я"c)
                                state = 0
                            Case "b"c
                                sb.Append("y"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("y"c)
                                state = 2
                            Case "d"c
                                sb.Append("y"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("ѣ"c)
                                state = 0
                            Case "f"c
                                sb.Append("y"c)
                                state = 6
                            Case "g"c
                                sb.Append("y"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("ѵ"c)
                                state = 0
                            Case "i"c
                                sb.Append("y"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("y"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("y"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("y"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("y"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("y"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("ё"c)
                                state = 0
                            Case "p"c
                                sb.Append("y"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("y"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("y"c)
                                state = 8
                            Case "t"c
                                sb.Append("y"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("ю"c)
                                state = 0
                            Case "v"c
                                sb.Append("y"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("y"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("y"c)
                            Case "z"c
                                sb.Append("y"c)
                                state = 12
                            Case Else
                                sb.Append("y"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 11

                        Select Case c
                            Case "#"c
                                sb.Append("Y"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("Y"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("Y"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("Y"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("Y"c)
                                state = 3
                            Case "D"c
                                sb.Append("Y"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("Y"c)
                                state = 5
                            Case "F"c
                                sb.Append("Y"c)
                                state = 7
                            Case "G"c
                                sb.Append("Y"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("Y"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("Y"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("Y"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("Y"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("Y"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("Y"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("Y"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("Y"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("Y"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("Y"c)
                                state = 9
                            Case "T"c
                                sb.Append("Y"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("Y"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("Y"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("Y"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("Y"c)
                            Case "Z"c
                                sb.Append("Y"c)
                                state = 13
                            Case "`"c
                                sb.Append("Ы"c)
                                state = 0
                            Case "a"c
                                sb.Append("Я"c)
                                state = 0
                            Case "b"c
                                sb.Append("Y"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("Y"c)
                                state = 2
                            Case "d"c
                                sb.Append("Y"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("Ѣ"c)
                                state = 0
                            Case "f"c
                                sb.Append("Y"c)
                                state = 6
                            Case "g"c
                                sb.Append("Y"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("Ѵ"c)
                                state = 0
                            Case "i"c
                                sb.Append("Y"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("Y"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("Y"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("Y"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("Y"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("Y"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("Ё"c)
                                state = 0
                            Case "p"c
                                sb.Append("Y"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("Y"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("Y"c)
                                state = 8
                            Case "t"c
                                sb.Append("Y"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("Ю"c)
                                state = 0
                            Case "v"c
                                sb.Append("Y"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("Y"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("Y"c)
                                state = 10
                            Case "z"c
                                sb.Append("Y"c)
                                state = 12
                            Case Else
                                sb.Append("Y"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 12

                        Select Case c
                            Case "#"c
                                sb.Append("з"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("з"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("з"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("з"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("з"c)
                                state = 3
                            Case "D"c
                                sb.Append("з"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("з"c)
                                state = 5
                            Case "F"c
                                sb.Append("з"c)
                                state = 7
                            Case "G"c
                                sb.Append("з"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("з"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("з"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("з"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("з"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("з"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("з"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("з"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("з"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("з"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("з"c)
                                state = 9
                            Case "T"c
                                sb.Append("з"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("з"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("з"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("з"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("з"c)
                                state = 11
                            Case "Z"c
                                sb.Append("з"c)
                                state = 13
                            Case "`"c
                                sb.Append("з"c)
                                state = 1
                            Case "a"c
                                sb.Append("з"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("з"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("з"c)
                                state = 2
                            Case "d"c
                                sb.Append("з"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("з"c)
                                state = 4
                            Case "f"c
                                sb.Append("з"c)
                                state = 6
                            Case "g"c
                                sb.Append("з"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("ж"c)
                                state = 0
                            Case "i"c
                                sb.Append("з"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("з"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("з"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("з"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("з"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("з"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("з"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("з"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("з"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("з"c)
                                state = 8
                            Case "t"c
                                sb.Append("з"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("з"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("з"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("з"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("з"c)
                                state = 10
                            Case "z"c
                                sb.Append("з"c)
                            Case Else
                                sb.Append("з"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 13

                        Select Case c
                            Case "#"c
                                sb.Append("З"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("З"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("З"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("З"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("З"c)
                                state = 3
                            Case "D"c
                                sb.Append("З"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("З"c)
                                state = 5
                            Case "F"c
                                sb.Append("З"c)
                                state = 7
                            Case "G"c
                                sb.Append("З"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("З"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("З"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("З"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("З"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("З"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("З"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("З"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("З"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("З"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("З"c)
                                state = 9
                            Case "T"c
                                sb.Append("З"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("З"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("З"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("З"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("З"c)
                                state = 11
                            Case "Z"c
                                sb.Append("З"c)
                            Case "`"c
                                sb.Append("З"c)
                                state = 1
                            Case "a"c
                                sb.Append("З"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("З"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("З"c)
                                state = 2
                            Case "d"c
                                sb.Append("З"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("З"c)
                                state = 4
                            Case "f"c
                                sb.Append("З"c)
                                state = 6
                            Case "g"c
                                sb.Append("З"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("Ж"c)
                                state = 0
                            Case "i"c
                                sb.Append("З"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("З"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("З"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("З"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("З"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("З"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("З"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("З"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("З"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("З"c)
                                state = 8
                            Case "t"c
                                sb.Append("З"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("З"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("З"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("З"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("З"c)
                                state = 10
                            Case "z"c
                                sb.Append("З"c)
                                state = 12
                            Case Else
                                sb.Append("З"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 14

                        Select Case c
                            Case "#"c
                                sb.Append("ш"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("ш"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("ш"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("ш"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("ш"c)
                                state = 3
                            Case "D"c
                                sb.Append("ш"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("ш"c)
                                state = 5
                            Case "F"c
                                sb.Append("ш"c)
                                state = 7
                            Case "G"c
                                sb.Append("ш"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("ш"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("ш"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("ш"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("ш"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("ш"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("ш"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("ш"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("ш"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("ш"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("ш"c)
                                state = 9
                            Case "T"c
                                sb.Append("ш"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("ш"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("ш"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("ш"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("ш"c)
                                state = 11
                            Case "Z"c
                                sb.Append("ш"c)
                                state = 13
                            Case "`"c
                                sb.Append("ш"c)
                                state = 1
                            Case "a"c
                                sb.Append("ш"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("ш"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("ш"c)
                                state = 2
                            Case "d"c
                                sb.Append("ш"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("ш"c)
                                state = 4
                            Case "f"c
                                sb.Append("ш"c)
                                state = 6
                            Case "g"c
                                sb.Append("ш"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("щ"c)
                                state = 0
                            Case "i"c
                                sb.Append("ш"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("ш"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("ш"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("ш"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("ш"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("ш"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("ш"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("ш"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("ш"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("ш"c)
                                state = 8
                            Case "t"c
                                sb.Append("ш"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("ш"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("ш"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("ш"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("ш"c)
                                state = 10
                            Case "z"c
                                sb.Append("ш"c)
                                state = 12
                            Case Else
                                sb.Append("ш"c)
                                sb.Append(c)
                                state = 0
                        End Select

                    Case 15

                        Select Case c
                            Case "#"c
                                sb.Append("Ш"c)
                                sb.Append("№"c)
                                state = 0
                            Case "'"c
                                sb.Append("Ш"c)
                                sb.Append("’"c)
                                state = 0
                            Case "A"c
                                sb.Append("Ш"c)
                                sb.Append("А"c)
                                state = 0
                            Case "B"c
                                sb.Append("Ш"c)
                                sb.Append("Б"c)
                                state = 0
                            Case "C"c
                                sb.Append("Ш"c)
                                state = 3
                            Case "D"c
                                sb.Append("Ш"c)
                                sb.Append("Д"c)
                                state = 0
                            Case "E"c
                                sb.Append("Ш"c)
                                state = 5
                            Case "F"c
                                sb.Append("Ш"c)
                                state = 7
                            Case "G"c
                                sb.Append("Ш"c)
                                sb.Append("Г"c)
                                state = 0
                            Case "I"c
                                sb.Append("Ш"c)
                                sb.Append("И"c)
                                state = 0
                            Case "J"c
                                sb.Append("Ш"c)
                                sb.Append("Й"c)
                                state = 0
                            Case "K"c
                                sb.Append("Ш"c)
                                sb.Append("К"c)
                                state = 0
                            Case "L"c
                                sb.Append("Ш"c)
                                sb.Append("Л"c)
                                state = 0
                            Case "M"c
                                sb.Append("Ш"c)
                                sb.Append("М"c)
                                state = 0
                            Case "N"c
                                sb.Append("Ш"c)
                                sb.Append("Н"c)
                                state = 0
                            Case "O"c
                                sb.Append("Ш"c)
                                sb.Append("О"c)
                                state = 0
                            Case "P"c
                                sb.Append("Ш"c)
                                sb.Append("П"c)
                                state = 0
                            Case "R"c
                                sb.Append("Ш"c)
                                sb.Append("Р"c)
                                state = 0
                            Case "S"c
                                sb.Append("Ш"c)
                                state = 9
                            Case "T"c
                                sb.Append("Ш"c)
                                sb.Append("Т"c)
                                state = 0
                            Case "U"c
                                sb.Append("Ш"c)
                                sb.Append("У"c)
                                state = 0
                            Case "V"c
                                sb.Append("Ш"c)
                                sb.Append("В"c)
                                state = 0
                            Case "X"c
                                sb.Append("Ш"c)
                                sb.Append("Х"c)
                                state = 0
                            Case "Y"c
                                sb.Append("Ш"c)
                                state = 11
                            Case "Z"c
                                sb.Append("Ш"c)
                                state = 13
                            Case "`"c
                                sb.Append("Ш"c)
                                state = 1
                            Case "a"c
                                sb.Append("Ш"c)
                                sb.Append("а"c)
                                state = 0
                            Case "b"c
                                sb.Append("Ш"c)
                                sb.Append("б"c)
                                state = 0
                            Case "c"c
                                sb.Append("Ш"c)
                                state = 2
                            Case "d"c
                                sb.Append("Ш"c)
                                sb.Append("д"c)
                                state = 0
                            Case "e"c
                                sb.Append("Ш"c)
                                state = 4
                            Case "f"c
                                sb.Append("Ш"c)
                                state = 6
                            Case "g"c
                                sb.Append("Ш"c)
                                sb.Append("г"c)
                                state = 0
                            Case "h"c
                                sb.Append("Щ"c)
                                state = 0
                            Case "i"c
                                sb.Append("Ш"c)
                                sb.Append("и"c)
                                state = 0
                            Case "j"c
                                sb.Append("Ш"c)
                                sb.Append("й"c)
                                state = 0
                            Case "k"c
                                sb.Append("Ш"c)
                                sb.Append("к"c)
                                state = 0
                            Case "l"c
                                sb.Append("Ш"c)
                                sb.Append("л"c)
                                state = 0
                            Case "m"c
                                sb.Append("Ш"c)
                                sb.Append("м"c)
                                state = 0
                            Case "n"c
                                sb.Append("Ш"c)
                                sb.Append("н"c)
                                state = 0
                            Case "o"c
                                sb.Append("Ш"c)
                                sb.Append("о"c)
                                state = 0
                            Case "p"c
                                sb.Append("Ш"c)
                                sb.Append("п"c)
                                state = 0
                            Case "r"c
                                sb.Append("Ш"c)
                                sb.Append("р"c)
                                state = 0
                            Case "s"c
                                sb.Append("Ш"c)
                                state = 8
                            Case "t"c
                                sb.Append("Ш"c)
                                sb.Append("т"c)
                                state = 0
                            Case "u"c
                                sb.Append("Ш"c)
                                sb.Append("у"c)
                                state = 0
                            Case "v"c
                                sb.Append("Ш"c)
                                sb.Append("в"c)
                                state = 0
                            Case "x"c
                                sb.Append("Ш"c)
                                sb.Append("х"c)
                                state = 0
                            Case "y"c
                                sb.Append("Ш"c)
                                state = 10
                            Case "z"c
                                sb.Append("Ш"c)
                                state = 12
                            Case Else
                                sb.Append("Ш"c)
                                sb.Append(c)
                                state = 0
                        End Select
                End Select
            Next

            Select Case state
                Case 1
                    sb.Append("ь"c)
                Case 2
                    sb.Append("ц"c)
                Case 3
                    sb.Append("Ц"c)
                Case 4
                    sb.Append("е"c)
                Case 5
                    sb.Append("Е"c)
                Case 6
                    sb.Append("ф"c)
                Case 7
                    sb.Append("Ф"c)
                Case 8
                    sb.Append("с"c)
                Case 9
                    sb.Append("С"c)
                Case 10
                    sb.Append("y"c)
                Case 11
                    sb.Append("Y"c)
                Case 12
                    sb.Append("з"c)
                Case 13
                    sb.Append("З"c)
                Case 14
                    sb.Append("ш"c)
                Case 15
                    sb.Append("Ш"c)
            End Select

            Return sb.ToString()
        End Using
    End Function
End Class
