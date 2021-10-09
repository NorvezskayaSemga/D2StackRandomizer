Imports System.Drawing
Imports System.ComponentModel
Imports System.Threading.Tasks

Public Class TemplateForge

    Private allParameters() As Parameter
    Public blocks(-1) As OptionsStorage
    Public allowToAddNewLocatons As Boolean = True

    ''' <summary>Нжно ли обновить список блоков</summary>
    Public reloadMe As Boolean

    Private Const isNonhideble As String = "Nonhideable"
    Private Const SymmetryClass As String = "SymmetryClass"
    'ReadFromFile example_template_2_unsymm.txt $cms

    Private Enum BlockType
        Main = 1
        Common = 2
        Location = 3
    End Enum

    Public Sub New(ByRef descriptionLanguage As GenDefaultValues.TextLanguage)

        allParameters = GetPermissibleParametersRange(descriptionLanguage)

        Call AddToArray(AddMainBlock)
        Call AddToArray(AddCommonBlock)
        For i As Integer = 0 To 1 Step 1
            Call AddLocation()
        Next i
        For j As Integer = 0 To UBound(blocks) Step 1
            For i As Integer = 0 To blocks(j).OptionsCount - 1 Step 1
                Call ValueChanged(blocks(j).name, blocks(j).GetOption(i).name)
            Next i
        Next j
        Call HideStateChanged()
        Call ToTextArray()
    End Sub

    Private Function AddMainBlock() As OptionsStorage
        Return CreateBlock(My.Resources.template_creation, My.Resources.template_creation, False, False, False)
    End Function
    Private Function AddCommonBlock() As OptionsStorage
        Return CreateBlock(My.Resources.template_map, My.Resources.template_map, False, True, False)
    End Function
    Private Function AddLocationBlock() As OptionsStorage
        Dim name As String = My.Resources.template_location & "_" & SingleToInt(Rnd)
        Do While GetBlockIndex(name, False) > -1
            name = My.Resources.template_location & "_" & SingleToInt(Rnd)
        Loop
        Return CreateBlock(My.Resources.template_location, name, True, True, True)
    End Function
    ''' <summary>Добавит новую локацию. Вернет название локации</summary>
    Public Function AddLocation() As String
        If Not allowToAddNewLocatons Then
            Throw New Exception("New locations adding isn't allowed")
            Return ""
        End If
        Call AddToArray(AddLocationBlock)
        Call ValueChanged(My.Resources.template_creation, "genMode")
        Return blocks(UBound(blocks)).name
    End Function
    Private Function SingleToInt(ByRef v As Single) As Integer
        Return BitConverter.ToInt32(BitConverter.GetBytes(v), 0)
    End Function
    Private Function CreateBlock(ByRef baseName As String, ByRef fullName As String, _
                                 ByRef canBeDeleted As Boolean, ByRef AddReadCommand As Boolean, _
                                 ByRef vRandomization As Boolean) As OptionsStorage
        Dim r As New OptionsStorage With {.canBeDeleted = canBeDeleted, .name = fullName}
        If fullName = My.Resources.template_creation Then
            r.blockTypeID = OptionsStorage.BlockType.CreationSettings
        ElseIf fullName = My.Resources.template_map Then
            r.blockTypeID = OptionsStorage.BlockType.CommonSettings
        Else
            r.blockTypeID = OptionsStorage.BlockType.LocationSettings
        End If
        Dim add As Boolean
        For Each p As Parameter In allParameters
            If Not IsNothing(p.blockName) Then
                add = False
                For Each n As String In p.blockName
                    If n.ToUpper = baseName.ToUpper Then
                        add = True
                        Exit For
                    End If
                Next n
                If add Then
                    If p.type = Parameter.ValueType.vBoolean _
                    Or p.type = Parameter.ValueType.vDouble _
                    Or p.type = Parameter.ValueType.vInteger Then
                        p.valueRandomization = vRandomization
                    Else
                        p.valueRandomization = False
                    End If
                    r.AddOption(p)
                    If p.valueRandomization Then
                        r.SetOptionValue(p.name, p.minValue, p.maxValue)
                    Else
                        r.SetOptionValue(p.name, p.minValue)
                    End If
                End If
            End If
        Next p
        Return r
    End Function
    Private Sub AddToArray(ByRef block As OptionsStorage)
        ReDim Preserve blocks(blocks.Length)
        blocks(UBound(blocks)) = block
    End Sub
    ''' <summary>Вернет индекс блока по его имени</summary>
    ''' <param name="name">Имя блока. Регистр игнорируется</param>
    ''' <param name="riseExceptionIfNotFound">Выдать ошибку, если такой локации нет</param>
    Public Function GetBlockIndex(ByRef name As String, Optional ByRef riseExceptionIfNotFound As Boolean = True) As Integer
        Dim n As String = name.ToUpper
        For i As Integer = 0 To UBound(blocks) Step 1
            If blocks(i).name.ToUpper = n Then Return i
        Next i
        If riseExceptionIfNotFound Then Throw New Exception("Could not find block with name: " & name)
        Return -1
    End Function

    ''' <summary>Установит для параметра в заданном блоке новое значение</summary>
    ''' <param name="blockName">Название блока</param>
    ''' <param name="valueName">Название параметра</param>
    ''' <param name="newValue">Новое значение</param>
    Public Sub SetValue(ByRef blockName As String, ByRef valueName As String, ByRef newValue As String)
        Call SetValue(GetBlockIndex(blockName), valueName, newValue)
    End Sub
    ''' <summary>Установит для параметра в заданном блоке новое значение</summary>
    ''' <param name="blockIndex">Индекс блока</param>
    ''' <param name="valueName">Название параметра</param>
    ''' <param name="newValue">Новое значение</param>
    Public Sub SetValue(ByRef blockIndex As Integer, ByRef valueName As String, ByRef newValue As String)
        Call blocks(blockIndex).SetOptionValue(valueName, newValue)
        Call ValueChanged(blocks(blockIndex).name, valueName)
    End Sub
    ''' <summary>Установит для параметра в заданном блоке новое значение</summary>
    ''' <param name="blockName">Название блока</param>
    ''' <param name="newvalueLowerBound">При создании карты генератор может рандомить параметры локаций. Это минимальное значение при рандомизации</param>>
    ''' <param name="newvalueUpperBound">При создании карты генератор может рандомить параметры локаций. Это максимальное значение при рандомизации</param>
    Public Sub SetValue(ByRef blockName As String, ByRef valueName As String, ByRef newValueLowerBound As String, ByRef newValueUpperBound As String)
        Call SetValue(GetBlockIndex(blockName), valueName, newValueLowerBound, newValueUpperBound)
    End Sub
    ''' <summary>Установит для параметра в заданном блоке новое значение</summary>
    ''' <param name="blockIndex">Индекс блока</param>
    ''' <param name="newvalueLowerBound">При создании карты генератор может рандомить параметры локаций. Это минимальное значение при рандомизации</param>>
    ''' <param name="newvalueUpperBound">При создании карты генератор может рандомить параметры локаций. Это максимальное значение при рандомизации</param>
    Public Sub SetValue(ByRef blockIndex As Integer, ByRef valueName As String, ByRef newValueLowerBound As String, ByRef newValueUpperBound As String)
        Call blocks(blockIndex).SetOptionValue(valueName, newValueLowerBound, newValueUpperBound)
        Call ValueChanged(blocks(blockIndex).name, valueName)
    End Sub
    ''' <summary>Показать или скрыть параметр</summary>
    ''' <param name="blockName">Название блока</param>
    ''' <param name="valueName">Название параметра</param>
    ''' <param name="newState">True, если скрыть</param>
    Public Sub SetHideValueState(ByRef blockName As String, ByRef valueName As String, ByRef newState As Boolean)
        Call SetHideValueState(GetBlockIndex(blockName), valueName, newState)
    End Sub
    ''' <summary>Показать или скрыть параметр</summary>
    ''' <param name="blockIndex">Индекс блока</param>
    ''' <param name="valueName">Название параметра</param>
    ''' <param name="newState">True, если скрыть</param>
    Public Sub SetHideValueState(ByRef blockIndex As Integer, ByRef valueName As String, ByRef newState As Boolean)
        Call blocks(blockIndex).SetOptionHideState(valueName, newState)
        Call HideStateChanged()
    End Sub
    Private Sub HideStateChanged()
        'Dim r As Integer
        'Dim show As Boolean
        'For i As Integer = 0 To UBound(blocks) Step 1
        '    r = blocks(i).OptionIndex(readCommand, False)
        '    If r > -1 Then
        '        show = False
        '        For j As Integer = 0 To blocks(i).OptionsCount - 1 Step 1
        '            If Not j = r Then
        '                If blocks(i).GetOption(j).hidden Then
        '                    show = True
        '                    Exit For
        '                End If
        '            End If
        '        Next j
        '        If blocks(i).GetOption(r).hidden = show Then
        '            Call blocks(i).SetOptionHideState(readCommand, Not show)
        '            'blocks(i).reloadMe = True
        '        End If
        '    End If
        'Next i
    End Sub
    Private Sub ValueChanged(ByRef blockName As String, ByRef valueName As String)
        Dim bIndex As Integer = GetBlockIndex(blockName)
        Dim v As String = blocks(bIndex).GetOption(valueName).valueLowerBound
        If blockName.ToUpper = My.Resources.template_creation.ToUpper Then
            If valueName.ToUpper = "genMode".ToUpper Then
                If CInt(v) = 1 Then
                    Dim locCount As Integer = 0
                    For i As Integer = 0 To UBound(blocks) Step 1
                        If blocks(i).name.ToUpper.StartsWith(My.Resources.template_location.ToUpper) Then
                            locCount += 1
                            If locCount > 2 Then
                                If blocks(i).hidden = False Then reloadMe = True
                                blocks(i).hidden = True
                            Else
                                If blocks(i).hidden = True Then reloadMe = True
                                blocks(i).hidden = False
                            End If
                        End If
                    Next i
                    If locCount < 2 Then
                        allowToAddNewLocatons = True
                    Else
                        allowToAddNewLocatons = False
                    End If
                Else
                    allowToAddNewLocatons = True
                    For i As Integer = 0 To UBound(blocks) Step 1
                        If blocks(i).name.ToUpper.StartsWith(My.Resources.template_location.ToUpper) Then
                            If blocks(i).hidden = True Then reloadMe = True
                            blocks(i).hidden = False
                        End If
                    Next i
                End If
            ElseIf valueName.ToUpper = "ApplySymmetry".ToUpper Then
                Dim hidden As String = blocks(bIndex).GetOption(SymmetryClass).hidden.ToString.ToUpper
                If v.ToUpper = "True".ToUpper Then
                    If hidden = "True".ToUpper Then
                        Call SetHideValueState(blockName, SymmetryClass, False)
                        blocks(bIndex).reloadMe = True
                    End If
                Else
                    If hidden = "False".ToUpper Then
                        Call SetHideValueState(blockName, SymmetryClass, True)
                        blocks(bIndex).reloadMe = True
                    End If
                End If
            End If
        End If
    End Sub

    ''' <summary>Удалит локацию. Изменит названия локаций, которые находились ниже</summary>
    ''' <param name="name">Название блона с локацией</param>
    Public Sub RemoveBlock(ByRef name As String)
        Call RemoveBlock(GetBlockIndex(name))
    End Sub
    ''' <summary>Удалит локацию. Изменит названия локаций, которые находились ниже</summary>
    ''' <param name="index">Индекс блона с локацией</param>
    Public Sub RemoveBlock(ByRef index As Integer)
        If Not blocks(index).canBeDeleted Then
            Throw New Exception("Deleting is not allowed")
            Exit Sub
        End If
        For i As Integer = index + 1 To UBound(blocks) Step 1
            blocks(i - 1) = blocks(i)
            'blocks(i - 1).reloadMe = True
        Next i
        ReDim Preserve blocks(UBound(blocks) - 1)
        Call ValueChanged(My.Resources.template_creation, "genMode")
    End Sub
    ''' <summary>Переместит блок выше по списку. Вернет False, если блок уже был наверху списка</summary>
    ''' <param name="name">Имя блока</param>
    Public Function MoveUpBlock(ByRef name As String) As Boolean
        Dim n As Integer = GetBlockIndex(name)
        Return MoveUpBlock(n)
    End Function
    ''' <summary>Переместит блок выше по списку. Вернет False, если блок уже был наверху списка</summary>
    ''' <param name="index">Индекс блока</param>
    Public Function MoveUpBlock(ByRef index As Integer) As Boolean
        Dim exchangeWith As Integer = -1
        For i As Integer = index - 1 To 0 Step -1
            If Not blocks(i).hidden Then
                exchangeWith = i
                Exit For
            End If
        Next i
        If exchangeWith > -1 Then
            Call ExchangeBlockPosition(exchangeWith, index)
            Return True
        Else
            Return False
        End If
    End Function
    ''' <summary>Переместит блок ниже по списку. Вернет False, если блок уже был внизу списка</summary>
    ''' <param name="name">Имя блока</param>
    Public Function MoveDownBlock(ByRef name As String) As Boolean
        Dim n As Integer = GetBlockIndex(name)
        Return MoveDownBlock(n)
    End Function
    ''' <summary>Переместит блок ниже по списку. Вернет False, если блок уже был внизу списка</summary>
    ''' <param name="index">Индекс блока</param>
    Public Function MoveDownBlock(ByRef index As Integer) As Boolean
        Dim exchangeWith As Integer = -1
        For i As Integer = index + 1 To UBound(blocks) Step 1
            If Not blocks(i).hidden Then
                exchangeWith = i
                Exit For
            End If
        Next i
        If exchangeWith > -1 Then
            Call ExchangeBlockPosition(exchangeWith, index)
            Return True
        Else
            Return False
        End If
    End Function
    Private Sub ExchangeBlockPosition(ByRef n1 As Integer, ByRef n2 As Integer)
        Dim b1, b2 As OptionsStorage
        b1 = blocks(n1)
        'b1.reloadMe = True
        b2 = blocks(n2)
        'b2.reloadMe = True
        blocks(n1) = b2
        blocks(n2) = b1
    End Sub

    ''' <summary>Прочитает шаблон из файла</summary>
    ''' <param name="templatesFolder">Папка с шаблонами</param>
    ''' <param name="fileName">Имя файла</param>
    Public Function ReadTemplateFile(ByRef templatesFolder As String, ByRef fileName As String) As OptionsStorage()
        Dim main As OptionsStorage = ReadCreationSettingsFromFile(templatesFolder, fileName)
        Dim common As OptionsStorage = ReadCommonMapSettingsFromFile(templatesFolder, fileName)
        Dim locations() As OptionsStorage = ReadAllLocationFromFile(templatesFolder, fileName)

        Dim res(UBound(locations) + 2) As OptionsStorage
        res(0) = main
        res(1) = common
        For i As Integer = 0 To UBound(locations) Step 1
            res(i + 2) = locations(i)
        Next i
        Return res
    End Function

    ''' <summary>Прочитает блок Creation_settings из файла</summary>
    ''' <param name="templatesFolder">Папка с шаблонами</param>
    ''' <param name="fileName">Имя файла</param>
    Public Function ReadCreationSettingsFromFile(ByRef templatesFolder As String, ByRef fileName As String) As OptionsStorage
        Dim fileSettings As Dictionary(Of String, String) = ImpenetrableMeshGen.GenSettings.ReadRawData(templatesFolder & "\" & fileName)
        Return ToOptions(fileSettings, Nothing, BlockType.Main)
    End Function
    ''' <summary>Прочитает блок Common_map_settings карты из файла</summary>
    ''' <param name="templatesFolder">Папка с шаблонами</param>
    ''' <param name="fileName">Имя файла</param>
    Public Function ReadCommonMapSettingsFromFile(ByRef templatesFolder As String, ByRef fileName As String) As OptionsStorage
        Dim fileSettings As Dictionary(Of String, String) = Map.SettingsMap.ReadRawData(templatesFolder & "\" & fileName)
        Return ToOptions(fileSettings, Nothing, BlockType.Common)
    End Function
    ''' <summary>Прочитает блок Location из файла. Вернет Nothing, если не найдет блок</summary>
    ''' <param name="templatesFolder">Папка с шаблонами</param>
    ''' <param name="fileName">Метка блока</param>
    Public Function ReadLocationFromFile(ByRef templatesFolder As String, ByRef fileName As String, ByRef locationLabel As String) As OptionsStorage
        Dim MinFileSettings() As Map.SettingsLoc.ExtendedBlockData = Map.SettingsLoc.ReadRawData(templatesFolder & "\" & fileName, 1)
        Dim MaxFileSettings() As Map.SettingsLoc.ExtendedBlockData = Map.SettingsLoc.ReadRawData(templatesFolder & "\" & fileName, 2)
        For i As Integer = 0 To UBound(MinFileSettings) Step 1
            If MinFileSettings(i).label.ToUpper = locationLabel.ToUpper Then
                Return ToOptions(MinFileSettings(i).data, MaxFileSettings(i).data, BlockType.Location)
            End If
        Next i
        Return Nothing
    End Function
    ''' <summary>Прочитает все блоки Location из файла</summary>
    ''' <param name="templatesFolder">Папка с шаблонами</param>
    Public Function ReadAllLocationFromFile(ByRef templatesFolder As String, ByRef fileName As String) As OptionsStorage()
        Dim MinFileSettings() As Map.SettingsLoc.ExtendedBlockData = Map.SettingsLoc.ReadRawData(templatesFolder & "\" & fileName, 1)
        Dim MaxFileSettings() As Map.SettingsLoc.ExtendedBlockData = Map.SettingsLoc.ReadRawData(templatesFolder & "\" & fileName, 2)
        Dim locations(UBound(MinFileSettings)) As OptionsStorage
        For i As Integer = 0 To UBound(MinFileSettings) Step 1
            locations(i) = ToOptions(MinFileSettings(i).data, MaxFileSettings(i).data, BlockType.Location)
        Next i
        Return locations
    End Function

    Private Function ToOptions(ByRef dataLower() As Dictionary(Of String, String), _
                               ByRef dataUpper() As Dictionary(Of String, String), _
                               ByRef blockID As BlockType) As OptionsStorage()
        Dim r(UBound(dataLower)) As OptionsStorage
        For i As Integer = 0 To UBound(dataLower) Step 1
            If IsNothing(dataUpper) Then
                r(i) = ToOptions(dataLower(i), Nothing, blockID)
            Else
                r(i) = ToOptions(dataLower(i), dataUpper(i), blockID)
            End If
        Next i
        Return r
    End Function
    Private Function ToOptions(ByRef dataLower() As Map.SettingsLoc.ExtendedBlockData, _
                               ByRef dataUpper() As Map.SettingsLoc.ExtendedBlockData, _
                               ByRef blockID As BlockType) As OptionsStorage()
        Dim r(UBound(dataLower)) As OptionsStorage
        For i As Integer = 0 To UBound(dataLower) Step 1
            If IsNothing(dataUpper) Then
                r(i) = ToOptions(dataLower(i).data, Nothing, blockID)
            Else
                r(i) = ToOptions(dataLower(i).data, dataUpper(i).data, blockID)
            End If
        Next i
        Return r
    End Function
    Private Function ToOptions(ByRef dataLower As Dictionary(Of String, String), _
                               ByRef dataUpper As Dictionary(Of String, String), _
                               ByRef blockID As BlockType) As OptionsStorage
        Dim res As OptionsStorage
        If blockID = BlockType.Main Then
            res = AddMainBlock()
        ElseIf blockID = BlockType.Common Then
            res = AddCommonBlock()
        ElseIf blockID = BlockType.Location Then
            res = AddLocationBlock()
        Else
            Throw New Exception("Unknown block id")
        End If
        For Each k As String In dataLower.Keys
            If IsNothing(dataUpper) Then
                res.SetOptionValue(k, dataLower.Item(k))
            Else
                res.SetOptionValue(k, dataLower.Item(k), dataUpper.Item(k))
            End If
        Next k
        Return res
    End Function

    Public Function ToTextArray() As String()
        Dim header As String = ""
        For Each p As Parameter In allParameters
            If ("%" & My.Resources.template_new_Block & My.Resources.template_header_keyword).ToUpper = p.name.ToUpper Then
                header = p.description
                Exit For
            End If
        Next p
        Dim textBlocks(UBound(blocks))() As String
        Dim outLen As Integer
        For i As Integer = 0 To UBound(blocks) Step 1
            textBlocks(i) = blocks(i).ToTextArray(allParameters)
            outLen += textBlocks(i).Length + 2
        Next i
        Dim out(outLen) As String
        out(0) = header
        Dim n As Integer = 0
        For i As Integer = 0 To UBound(textBlocks) Step 1
            For j As Integer = 0 To 1 Step 1
                n += 1
                out(n) = ""
            Next j
            For j As Integer = 0 To UBound(textBlocks(i)) Step 1
                n += 1
                out(n) = textBlocks(i)(j)
            Next j
        Next i
        Return out
    End Function

    'создать группы параметров при вызове new:
    'mode, карта
    'по запросу создаль группу для локации или удалить
    'по запросу записать в файл (tmp_(rnd while exist)) и вернуть путь к файлу
    'при изменении мода изменять видимость полей
    'при изменении симметрии изменять список доступных симметрий

    Public Structure Parameter
        ''' <summary>Название параметра</summary>
        Public name As String
        ''' <summary>Описание параметра</summary>
        Public description As String
        ''' <summary>Тип значения</summary>
        Public type As ValueType

        ''' <summary>Минимальное значение, если это не массив</summary>
        Public minValue As String
        ''' <summary>Максимальное значение, если это не массив</summary>
        Public maxValue As String

        ''' <summary>Возможные наборы параметров, если тип значения - массив. Можно выбирать только из одного наборы.
        ''' Например: если что-то выбрано из possibleArrayValues(0), то нельзя брать из possibleArrayValues(1)</summary>
        Public possibleArrayValues()() As String
        ''' <summary>Разделитель значений в случае, если параметр является массивом</summary>
        Public arrayDelimiter As String

        ''' <summary>При создании карты генератор может рандомить параметры локаций. Это минимальное значение при рандомизации</summary>
        Public valueLowerBound As String
        ''' <summary>При создании карты генератор может рандомить параметры локаций. Это максимальное значение при рандомизации</summary>
        Public valueUpperBound As String
        ''' <summary>True, если при создании карты генератор будет рандомить этот параметр от valueLowerBound до valueUpperBound</summary>
        Public valueRandomization As Boolean

        ''' <summary>Скрывать параметр и не сохранять его в файл</summary>
        Public hidden As Boolean
        ''' <summary>Можно ли скрыть параметр и не сохранять его в файл</summary>
        Public hideable As Boolean

        ''' <summary>Базовые названия блоков, в которых может находиться параметр</summary>
        Public blockName() As String
        ''' <summary>Условие, при котором параметр должен быть виден</summary>
        Public showCondition As String

        Public Function ToTextArray(ByRef maxValueNameLen As Integer, _
                                    ByRef maxValueLowerBoundLen As Integer) As String()
            Dim s() As String = ValueConverter.TxtSplit(description)
            Dim out(UBound(s) + 3) As String
            out(0) = ""
            For i As Integer = 0 To UBound(s) Step 1
                out(i + 1) = "#" & s(i)
            Next i
            Dim n As Integer = UBound(s)
            n += 1

            If type = ValueType.vStringArray Then
                out(n) = "#" & wArray & " Permissible values are from arrays (don't use values from different arrays), delimiter is" & arrayDelimiter & ":"
                For i As Integer = 0 To UBound(possibleArrayValues) Step 1
                    ReDim Preserve out(out.Length)
                    n += 1
                    For j As Integer = 0 To UBound(possibleArrayValues(i)) Step 1
                        If out(n) = "" Then
                            out(n) = "#"
                        Else
                            out(n) &= arrayDelimiter
                        End If
                        out(n) &= possibleArrayValues(i)(j)
                    Next j
                Next i
            ElseIf type = ValueType.vString Then
                out(n) = "#" & wString
            Else
                out(n) = "#"
                If type = ValueType.vBoolean Then
                    out(n) &= wBoolean
                ElseIf type = ValueType.vDouble Then
                    out(n) &= wDouble
                ElseIf type = ValueType.vInteger Then
                    out(n) &= wInteger
                Else
                    Throw New Exception("Unexpected value type")
                End If
                out(n) &= " #Permissible values are from " & minValue & " to " & maxValue & " "
            End If
            n += 1
            out(n) = name
            Do While out(n).Length <= maxValueNameLen
                out(n) &= " "
            Loop
            out(n) &= valueLowerBound
            If valueRandomization And Not valueLowerBound = valueUpperBound And Not valueUpperBound = "" Then
                Do While out(n).Length <= maxValueNameLen + maxValueLowerBoundLen + 1
                    out(n) &= " "
                Loop
                out(n) &= valueUpperBound
            End If
            ReDim Preserve out(n)
            Return out
        End Function

        Enum ValueType
            vStringArray = 1
            vDouble = 2
            vInteger = 3
            vBoolean = 4
            vString = 5
        End Enum

        Friend Const wArray As String = "[StringArray]"
        Friend Const wDouble As String = "[Double]"
        Friend Const wInteger As String = "[Integer]"
        Friend Const wBoolean As String = "[Boolean]"
        Friend Const wString As String = "[String]"

    End Structure
    Public Shared Function GetPermissibleParametersRange(ByRef descriptionLanguage As GenDefaultValues.TextLanguage) As Parameter()
        Dim r() As String = ValueConverter.TxtSplit(My.Resources.GenParametersRange)
        Dim d() As String
        If descriptionLanguage = GenDefaultValues.TextLanguage.Rus Then
            d = ValueConverter.TxtSplit(My.Resources.GenParametersDescription_Rus)
        ElseIf descriptionLanguage = GenDefaultValues.TextLanguage.Eng Then
            d = ValueConverter.TxtSplit(My.Resources.GenParametersDescription_Eng)
        Else
            Throw New Exception("Unexpected language")
        End If
        Dim test() As String = ValueConverter.TxtSplit(My.Resources.example_template_1)

        Dim res(UBound(r)) As Parameter
        Dim nameField As Integer = 0
        Dim blockField As Integer = 1
        Dim typeField As Integer = 2
        Dim minField As Integer = 3
        Dim maxField As Integer = 4

        Dim splR(UBound(r))(), splD(UBound(d))() As String
        Dim testExampleNames, testParametersNames As New List(Of String)
        Dim testParametersDescriptions As New Dictionary(Of String, String)
        For i As Integer = 0 To UBound(r) Step 1
            splR(i) = r(i).Split(CChar(" "))
            Dim t As String = splR(i)(nameField).ToUpper
            If Not testParametersNames.Contains(t) Then
                testParametersNames.Add(t)
            Else
                Throw New Exception("Дублирование параметра " & splR(i)(nameField) & " в файле с допустимыми значениями (GenParametersRange.txt)")
            End If
        Next i
        For i As Integer = 0 To UBound(d) Step 1
            splD(i) = d(i).Split(CChar(" "))
            Dim t As String = splD(i)(nameField).ToUpper
            If Not testParametersDescriptions.ContainsKey(t) Then
                Dim desc As String = ""
                For k As Integer = 1 To UBound(splD(i)) Step 1
                    If k > 1 Then desc &= " "
                    desc &= splD(i)(k).Replace("/n", vbNewLine)
                Next k
                testParametersDescriptions.Add(t, desc)
            Else
                Throw New Exception("Дублирование параметра " & splD(i)(nameField) & " в файле с описаниями параметров (GenParametersDescriptions.txt)")
            End If
        Next i

        For i As Integer = 1 To UBound(test) Step 1
            Dim t As String = test(i).Split(CChar(" "))(nameField).ToUpper
            If Not t.StartsWith(My.Resources.template_new_Block) Then
                If Not testExampleNames.Contains(t) Then testExampleNames.Add(t)
            End If
        Next i
        For Each t As String In testParametersNames
            If Not testExampleNames.Contains(t) Then Throw New Exception("Не могу найти параметр " & t & " в тестовом шаблоне (example_template_1)")
        Next t
        For Each t As String In testExampleNames
            If Not testParametersNames.Contains(t) Then Throw New Exception("Не могу найти параметр " & t & " в файле с допустимыми значениями (GenParametersRange.txt)")
        Next t
        For Each t As String In testExampleNames
            If Not testParametersDescriptions.ContainsKey(t) Then Throw New Exception("Не могу найти параметр " & t & " в файле с описаниями параметров (GenParametersDescriptions.txt)")
        Next t

        For i As Integer = 0 To UBound(r) Step 1
            Dim blockSettings As String = splR(i)(blockField).Replace("[", "").Replace("]", "")
            ReDim res(i).blockName(-1)
            res(i).name = splR(i)(nameField)
            res(i).hideable = True
            If blockSettings.Contains(":") Then
                Dim s() As String = blockSettings.Split(CChar(":"))
                For j As Integer = 0 To UBound(s) Step 1
                    If s(j).ToUpper = My.Resources.template_creation.ToUpper _
                     Or s(j).ToUpper = My.Resources.template_map.ToUpper _
                     Or s(j).ToUpper = My.Resources.template_location.ToUpper Then
                        ReDim Preserve res(i).blockName(res(i).blockName.Length)
                        res(i).blockName(UBound(res(i).blockName)) = s(j)
                    ElseIf s(j).ToUpper = TemplateForge.isNonhideble.ToUpper Then
                        res(i).hideable = False
                    Else
                        res(i).showCondition = s(j)
                    End If
                Next j
            Else
                res(i).blockName = New String() {blockSettings}
            End If
            If splR(i)(typeField).ToUpper.StartsWith(Parameter.wArray.ToUpper) Then
                res(i).type = Parameter.ValueType.vStringArray
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wDouble.ToUpper) Then
                res(i).type = Parameter.ValueType.vDouble
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wInteger.ToUpper) Then
                res(i).type = Parameter.ValueType.vInteger
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wBoolean.ToUpper) Then
                res(i).type = Parameter.ValueType.vBoolean
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wString.ToUpper) Then
                res(i).type = Parameter.ValueType.vString
            Else
                Throw New Exception("Неожиданный тип переменной: " & splR(i)(1) & " параметра " & splR(i)(0))
            End If
            res(i).description = testParametersDescriptions.Item(res(i).name.ToUpper)
            If Not res(i).type = Parameter.ValueType.vStringArray Then
                res(i).minValue = splR(i)(minField)
                res(i).maxValue = splR(i)(maxField)
            Else
                Dim delimiters As String = splR(i)(typeField).Substring(Parameter.wArray.Length)
                Dim d1 As Char = delimiters(0)
                Dim d2 As Char = delimiters(1)
                Dim arrays() As String = splR(i)(minField).Split(d1)
                ReDim res(i).possibleArrayValues(UBound(arrays))
                For j As Integer = 0 To UBound(arrays) Step 1
                    res(i).possibleArrayValues(j) = arrays(j).Replace("[", "").Replace("]", "").Split(d2)
                Next j
                res(i).arrayDelimiter = d2
            End If
            Console.WriteLine(res(i).description)
            Console.WriteLine("----------------")
        Next i
        Dim info() As String = {My.Resources.template_header_keyword, _
                                My.Resources.template_creation, _
                                My.Resources.template_map, _
                                My.Resources.template_location}
        ReDim Preserve res(UBound(res) + info.Length)

        For i As Integer = 0 To UBound(info) Step 1
            Dim n As Integer = UBound(res) - UBound(info) + i
            res(n).name = "%" & My.Resources.template_new_Block & info(i)
            res(n).description = testParametersDescriptions.Item(res(n).name.ToUpper)
        Next i
        Return res
    End Function


    Public Class OptionsStorage

        Delegate Sub ValueChangedHandler(ByRef blockName As String, ByRef valueName As String)
        Delegate Sub HideChangedHandler()

        Private options(-1) As Parameter
        ''' <summary>Скрывать блок и не сохранять его в файл</summary>
        Public hidden As Boolean
        ''' <summary>Можно ли удалить блок</summary>
        Public canBeDeleted As Boolean
        ''' <summary>Нуждаются ли данные блока в обновлении в интерфейсе</summary>
        Public reloadMe As Boolean
        ''' <summary>Название блока</summary>
        Public name As String
        ''' <summary>Кастомный идентификатор блока. Без пробелов и табов. Опционально</summary>
        Private customBlockID As String

        Public blockTypeID As BlockType

        Public Enum BlockType
            CreationSettings = 1
            CommonSettings = 2
            LocationSettings = 3
        End Enum

        ''' <summary>Задать идентификатор блока. Опционально</summary>
        ''' <param name="v">Идентификатр без пробелов и табов.</param>
        Public Sub SetCustomBlockID(ByVal v As String)
            Dim r As String = v.Trim(CChar(" ")).Trim(CChar(vbTab))
            If r.Contains(" ") Or r.Contains(vbTab) Then
                Throw New Exception("Remove spacews and tabs from ID")
            Else
                customBlockID = r
            End If
        End Sub
        ''' <summary>Получить идентификатор блока</summary>
        Public Function GetCustomBlockID() As String
            Return customBlockID
        End Function


        ''' <summary>Показать или скрыть параметр</summary>
        ''' <param name="vName">Имя параметра. Регистр игнорируется</param>  
        ''' <param name="newState">True, если скрыть</param>
        Public Sub SetOptionHideState(ByVal vName As String, ByRef newState As Boolean, _
                                      Optional ByRef h As HideChangedHandler = Nothing)
            Dim index As Integer = OptionIndex(vName)
            options(index).hidden = newState
            If newState And Not options(index).hideable Then
                Throw New Exception("Hidden state for " & vName & " is not allowed")
            End If
            If Not IsNothing(h) Then Call h()
        End Sub

        ''' <summary>Вернет индекс параметра по его имени</summary>
        ''' <param name="vName">Имя параметра. Регистр игнорируется</param>  
        ''' <param name="riseExceptionIfNotFound">Выдать ошибку, если такого параметра нет</param>
        Public Function OptionIndex(ByVal vName As String, Optional ByRef riseExceptionIfNotFound As Boolean = True) As Integer
            Dim n As String = vName.ToUpper
            For i As Integer = 0 To UBound(options) Step 1
                If options(i).name.ToUpper = n Then Return i
            Next i
            If riseExceptionIfNotFound Then Throw New Exception("Could not find parameter with name: " & vName)
            Return -1
        End Function
        ''' <summary>Количество параметров</summary>
        Public Function OptionsCount() As Integer
            Return options.Length
        End Function

        ''' <summary>Установит значение параметра</summary>
        ''' <param name="vName">Имя параметра. Регистр игнорируется</param>
        ''' <param name="value">Значение</param>
        ''' <param name="h">Вызовет эту процедуру после изменения значния</param>
        Public Sub SetOptionValue(ByVal vName As String, ByVal value As String, _
                                  Optional ByRef h As ValueChangedHandler = Nothing)
            Dim index As Integer = OptionIndex(vName)
            If options(index).valueRandomization Then Throw New Exception("Use SetOptionValue with ""valueLowerBound"" and ""valueUpperBound""")
            options(index).valueLowerBound = value
            options(index).valueUpperBound = value
            If Not IsNothing(h) Then Call h(name, vName)
        End Sub
        ''' <summary>Установит значение параметра</summary>
        ''' <param name="vName">Имя параметра. Регистр игнорируется</param>
        ''' <param name="valueLowerBound">При создании карты генератор может рандомить параметры локаций. Это минимальное значение при рандомизации</param>>
        ''' <param name="valueUpperBound">При создании карты генератор может рандомить параметры локаций. Это максимальное значение при рандомизации</param>
        ''' <param name="h">Вызовет эту процедуру после изменения значния</param>
        Public Sub SetOptionValue(ByVal vName As String, ByVal valueLowerBound As String, ByVal valueUpperBound As String, _
                                  Optional ByRef h As ValueChangedHandler = Nothing)
            Dim index As Integer = OptionIndex(vName)
            If Not options(index).valueRandomization Then Throw New Exception("Use SetOptionValue with ""value""")
            options(index).valueLowerBound = valueLowerBound
            options(index).valueUpperBound = valueUpperBound
            If Not IsNothing(h) Then Call h(name, vName)
        End Sub

        ''' <summary>Вернет параметр по индексу</summary>
        ''' <param name="index">Индекс</param>
        Public Function GetOption(ByVal index As Integer) As Parameter
            Return options(index)
        End Function
        ''' <summary>Вернет параметр по имени</summary>
        ''' <param name="vName">Имя параметра</param>
        Public Function GetOption(ByVal vName As String) As Parameter
            Return GetOption(OptionIndex(vName))
        End Function

        ''' <summary>Добавит параметр в конец списка</summary>
        ''' <param name="value">Параметр</param>
        Public Sub AddOption(ByVal value As Parameter)
            ReDim Preserve options(OptionsCount)
            options(OptionsCount() - 1) = value
            'Call OnOptionsListChanged()
        End Sub

        Public Function ToTextArray(ByRef allParameters() As Parameter) As String()
            Dim header As String = ""
            Dim baseName As String
            If blockTypeID = BlockType.CreationSettings Then
                baseName = My.Resources.template_creation
            ElseIf blockTypeID = BlockType.CommonSettings Then
                baseName = My.Resources.template_map
            Else
                baseName = My.Resources.template_location
            End If
            For Each p As Parameter In allParameters
                If ("%" & My.Resources.template_new_Block & baseName).ToUpper = p.name.ToUpper Then
                    header = p.description
                    Exit For
                End If
            Next p

            Dim out() As String = {header, My.Resources.template_new_Block & baseName}
            Dim maxValueNameLen, maxValueLowerBoundLen As Integer
            For i As Integer = 0 To OptionsCount() - 1 Step 1
                If Not IsNothing(options(i).name) Then maxValueNameLen = Math.Max(maxValueNameLen, options(i).name.Length)
                If Not IsNothing(options(i).valueLowerBound) Then maxValueLowerBoundLen = Math.Max(maxValueLowerBoundLen, options(i).valueLowerBound.Length)
            Next i
            Dim s() As String
            Dim n As Integer = UBound(out)
            For i As Integer = 0 To OptionsCount() - 1 Step 1
                If Not options(i).hidden Then
                    s = options(i).ToTextArray(maxValueNameLen, maxValueLowerBoundLen)
                    ReDim Preserve out(n + s.Length)
                    For j As Integer = 0 To UBound(s) Step 1
                        n += 1
                        out(n) = s(j)
                    Next j
                End If
            Next i
            Return out
        End Function

    End Class

End Class

Public Class DefMapObjects

    Public Enum Types As Integer
        None = 0
        Capital = 1
        City = 2
        Vendor = 3
        Mercenary = 4
        Mage = 5
        Trainer = 6
        Ruins = 7
        Mine = 8
    End Enum
    Public Shared Function toObjType(ByRef v As Integer) As Types
        For Each i As Types In System.Enum.GetValues(GetType(Types))
            If CInt(i) = v Then Return i
        Next i
        Throw New Exception("Invalid attended object type")
    End Function

    Public Const townT1 As String = "G000FT0000NE1"
    Public Const townT2 As String = "G000FT0000NE2"
    Public Const townT3 As String = "G000FT0000NE3"
    Public Const townT4 As String = "G000FT0000NE4"
    Public Const townT5 As String = "G000FT0000NE5"

    Public Const mineGold As String = "G000CR0000GL"
    Public Const mineGreen As String = "G000CR0000GR"
    Public Const mineBlack As String = "G000CR0000RG"
    Public Const mineWhite As String = "G000CR0000WH"
    Public Const mineRed As String = "G000CR0000RD"
    Public Const mineBlue As String = "G000CR0000YE"
End Class

Public Class ImpenetrableMeshGen

    Public ReadOnly minLocationRadiusAtAll As Double

    Private Structure PrepareToRaceLocGenResult
        Dim DminimumDist As Double
        ''' <summary>Минимальное расстояние^2</summary>
        Dim IminimumDist As Integer
        Dim possiblePoints() As Point
        Dim ppIDs As List(Of Integer)
        Dim raceLocs() As Location
        Dim equalDist(,) As List(Of Integer)
    End Structure

    Private rndgen As New RndValueGen
    Protected Friend comm As Common
    Protected Friend symm As New SymmetryOperations
    Private stackLocGen As StackLocationsGen

    Public ActiveObjects() As AttendedObject
    Public ObjectBlank()(,) As Map.Cell

    Public Structure GenSettings

        Enum genModes
            ''' <summary>Будет использован блок Map.SettingsLoc и два блока Map.SettingsLoc - для стартовых и всех остальных локаций, соответственно</summary>
            simple = 1
            ''' <summary>Будет использован блок Map.SettingsLoc и все блоки Map.SettingsLoc - для каждой локации индивидуальные настройки</summary>
            template = 2
        End Enum

        ''' <summary>Режим генерации, см. genModes</summary>
        Dim genMode As Integer

        Dim common_settMap As Map.SettingsMap

        Dim simple_settRaceLoc As Map.SettingsLoc
        Dim simple_settCommLoc As Map.SettingsLoc

        Dim template_settLoc() As Map.SettingsLoc
        Dim template_settGenLoc() As LocationGenSetting

        Public Structure LocationGenSetting

            ''' <summary>Координата локации на карте по X (число будет умножена на размер карты по X)</summary>
            Public posX As ValueRange
            ''' <summary>Координата локации на карте по Y (число будет умножена на размер карты по Y)</summary>
            Public posY As ValueRange
            ''' <summary>Шанс на то, что локация появится. От 0 до 1</summary>
            Public AppearanceChance As ValueRange

            ''' <summary>Минимальные значения для параметров локации</summary>
            Public minValues As Map.SettingsLoc
            ''' <summary>Максимальные значения для параметров локации</summary>
            Public maxValues As Map.SettingsLoc

            Public Shared Function RandomizeSettings(ByRef min As Map.SettingsLoc, ByRef max As Map.SettingsLoc, ByRef randomizer As RndValueGen) As Map.SettingsLoc
                Dim res As New Map.SettingsLoc
                res.AverageRadius = RandomValue(min.AverageRadius, max.AverageRadius, randomizer)
                res.maxEccentricityDispersion = RandomValue(min.maxEccentricityDispersion, max.maxEccentricityDispersion, randomizer)
                res.maxRadiusDispersion = RandomValue(min.maxRadiusDispersion, max.maxRadiusDispersion, randomizer)
                res.DecorationsAmount = RandomValue(min.DecorationsAmount, max.DecorationsAmount, randomizer)
                res.maxGoldMines = RandomValue(min.maxGoldMines, max.maxGoldMines, randomizer)
                res.maxManaSources = RandomValue(min.maxManaSources, max.maxManaSources, randomizer)
                res.maxCities = RandomValue(min.maxCities, max.maxCities, randomizer)
                res.maxVendors = RandomValue(min.maxVendors, max.maxVendors, randomizer)
                res.maxMercenaries = RandomValue(min.maxMercenaries, max.maxMercenaries, randomizer)
                res.maxMages = RandomValue(min.maxMages, max.maxMages, randomizer)
                res.maxTrainers = RandomValue(min.maxTrainers, max.maxTrainers, randomizer)
                res.maxRuins = RandomValue(min.maxRuins, max.maxRuins, randomizer)
                res.minStackToStackDist = RandomValue(min.minStackToStackDist, max.minStackToStackDist, randomizer)
                res.expAmount = RandomValue(min.expAmount, max.expAmount, randomizer)
                res.mageSpellsMaxLevel = RandomValue(min.mageSpellsMaxLevel, max.mageSpellsMaxLevel, randomizer)
                res.mageSpellsMinLevel = RandomValue(min.mageSpellsMinLevel, max.mageSpellsMinLevel, randomizer)
                res.mageSpellsCount = RandomValue(min.mageSpellsCount, max.mageSpellsCount, randomizer)
                res.mageGlobalSpellsEnabled = RandomValue(min.mageGlobalSpellsEnabled, max.mageGlobalSpellsEnabled, randomizer)
                res.mercenariesMaxExpBar = RandomValue(min.mercenariesMaxExpBar, max.mercenariesMaxExpBar, randomizer)
                res.mercenariesMinExpBar = RandomValue(min.mercenariesMinExpBar, max.mercenariesMinExpBar, randomizer)
                res.mercenariesCount = RandomValue(min.mercenariesCount, max.mercenariesCount, randomizer)
                res.merchMaxConsumableItemCost = RandomValue(min.merchMaxConsumableItemCost, max.merchMaxConsumableItemCost, randomizer)
                res.merchMinConsumableItemCost = RandomValue(min.merchMinConsumableItemCost, max.merchMinConsumableItemCost, randomizer)
                res.merchMaxNonconsumableItemCost = RandomValue(min.merchMaxNonconsumableItemCost, max.merchMaxNonconsumableItemCost, randomizer)
                res.merchMinNonconsumableItemCost = RandomValue(min.merchMinNonconsumableItemCost, max.merchMinNonconsumableItemCost, randomizer)
                res.merchItemsCost = RandomValue(min.merchItemsCost, max.merchItemsCost, randomizer)
                res.scaleContent = RandomValue(min.scaleContent, max.scaleContent, randomizer)
                res.possibleRaces = RandomValue(min.possibleRaces, max.possibleRaces, randomizer)
                res.RaceCities = RandomValue(min.RaceCities, max.RaceCities, randomizer)
                res.ConnectWithAllNeighboringLocations = RandomValue(min.ConnectWithAllNeighboringLocations, max.ConnectWithAllNeighboringLocations, randomizer)
                res.ruinsPowerMultiplicator = RandomValue(min.ruinsPowerMultiplicator, max.ruinsPowerMultiplicator, randomizer)
                res.ruinsWealthMultiplicator = RandomValue(min.ruinsWealthMultiplicator, max.ruinsWealthMultiplicator, randomizer)
                res.citiesPowerMultiplicator = RandomValue(min.citiesPowerMultiplicator, max.citiesPowerMultiplicator, randomizer)
                res.citiesWealthMultiplicator = RandomValue(min.citiesWealthMultiplicator, max.citiesWealthMultiplicator, randomizer)

                res.mageSpellsMinLevel = Math.Min(res.mageSpellsMinLevel, res.mageSpellsMaxLevel)
                res.mercenariesMinExpBar = Math.Min(res.mercenariesMinExpBar, res.mercenariesMaxExpBar)
                res.merchMinConsumableItemCost = Math.Min(res.merchMinConsumableItemCost, res.merchMaxConsumableItemCost)
                res.merchMinNonconsumableItemCost = Math.Min(res.merchMinNonconsumableItemCost, res.merchMaxNonconsumableItemCost)

                Return res
            End Function

            Public Class ValueRange
                ''' <summary>Минимальное значение</summary>
                Public min As Double
                ''' <summary>Максимальное значение</summary>
                Public max As Double

                Public Function RandomValue(ByRef randomizer As RndValueGen) As Double
                    Return GenSettings.LocationGenSetting.RandomValue(min, max, randomizer)
                End Function

                Public Shared Function Copy(ByRef v As ValueRange) As ValueRange
                    Return New ValueRange With {.min = v.min, _
                                                .max = v.max}
                End Function
            End Class

            Public Shared Function RandomValue(ByRef min As Double, ByRef max As Double, ByRef randomizer As RndValueGen) As Double
                If min > max Then Throw New Exception("Один из параметров в шаблоне задан неверно. Минимальное значение (" & min & ") больше максимального (" & max & ")")
                If min = max Then
                    Return min
                Else
                    Return randomizer.Rand(min, max, True)
                End If
            End Function
            Public Shared Function RandomValue(ByRef min As Integer, ByRef max As Integer, ByRef randomizer As RndValueGen) As Integer
                If min > max Then Throw New Exception("Один из параметров в шаблоне задан неверно. Минимальное значение (" & min & ") больше максимального (" & max & ")")
                If min = max Then
                    Return min
                Else
                    Return randomizer.RndInt(min, max, True)
                End If
            End Function
            Public Shared Function RandomValue(ByRef min As Boolean, ByRef max As Boolean, ByRef randomizer As RndValueGen) As Boolean
                If min = max Then
                    Return min
                Else
                    If randomizer.RndInt(0, 1, True) = 0 Then
                        Return min
                    Else
                        Return max
                    End If
                End If
            End Function
            Public Shared Function RandomValue(ByRef min() As String, ByRef max() As String, ByRef randomizer As RndValueGen) As String()
                If randomizer.RndInt(0, 1, True) = 0 Then
                    If Not IsNothing(min) Then
                        Return CType(min.Clone, String())
                    Else
                        Return Nothing
                    End If
                Else
                    If Not IsNothing(max) Then
                        Return CType(max.Clone, String())
                    Else
                        Return Nothing
                    End If
                End If
            End Function
            Public Shared Function RandomValue(ByRef min() As Map.SettingsLoc.SettingsRaceCity, ByRef max() As Map.SettingsLoc.SettingsRaceCity, ByRef randomizer As RndValueGen) As Map.SettingsLoc.SettingsRaceCity()
                If randomizer.RndInt(0, 1, True) = 0 Then
                    If Not IsNothing(min) Then
                        Return Map.SettingsLoc.SettingsRaceCity.Copy(min)
                    Else
                        Return Nothing
                    End If
                Else
                    If Not IsNothing(max) Then
                        Return Map.SettingsLoc.SettingsRaceCity.Copy(max)
                    Else
                        Return Nothing
                    End If
                End If
            End Function

            Public Shared Sub ReadPos(ByRef path As String, ByRef locSettings() As LocationGenSetting)
                Dim txt() As String = ValueConverter.TxtSplit(IO.File.ReadAllText(path))
                For i As Integer = 0 To UBound(locSettings) Step 1
                    Call locSettings(i).Read(txt, i + 1, path)
                Next i
            End Sub
            Private Sub Read(ByRef txt() As String, ByRef blockNumber As Integer, ByRef baseFilePath As String)
                Dim minData As Dictionary(Of String, String) = Map.ReadBlock(txt, GenDefaultValues.wTemplate_LocationKeyword, _
                                                                             blockNumber, 1, True, baseFilePath, True)
                Dim maxData As Dictionary(Of String, String) = Map.ReadBlock(txt, GenDefaultValues.wTemplate_LocationKeyword, _
                                                                             blockNumber, 2, True, baseFilePath, True)

                Dim d() As Dictionary(Of String, String) = {minData, maxData}
                Dim m() As String = {"min", "max"}
                Dim v() As Map.SettingsLoc = {minValues, maxValues}

                posX = New ValueRange
                posY = New ValueRange
                AppearanceChance = New ValueRange

                For b As Integer = 0 To UBound(m) Step 1
                    For Each c As String In {"AppearanceChance", "posX", "posY"}
                        Call Map.ReadValue(ClassFieldsHandler.GetField(Me, c & "." & m(b)), d(b), GenDefaultValues.wTemplate_LocationKeyword, CChar(";"), c)
                    Next c
                    Call Map.ReadValue(ClassFieldsHandler.GetField(v(b), "RaceCities"), d(b), GenDefaultValues.wTemplate_LocationKeyword, CChar(";"))
                Next b

            End Sub

            Public Shared Function Copy(ByRef v As LocationGenSetting) As LocationGenSetting
                Return New LocationGenSetting With {.posX = ValueRange.Copy(v.posX), _
                                                    .posY = ValueRange.Copy(v.posY), _
                                                    .AppearanceChance = v.AppearanceChance, _
                                                    .minValues = Map.SettingsLoc.Copy(v.minValues), _
                                                    .maxValues = Map.SettingsLoc.Copy(v.maxValues)}
            End Function
            Public Shared Function Copy(ByRef v() As LocationGenSetting) As LocationGenSetting()
                If IsNothing(v) Then Return Nothing
                Dim r(UBound(v)) As LocationGenSetting
                For i As Integer = 0 To UBound(v) Step 1
                    r(i) = LocationGenSetting.Copy(v(i))
                Next i
                Return r
            End Function

        End Structure

        Public Shared Function Read(ByRef path As String) As GenSettings
            Dim res As New GenSettings
            Call res.ReadMode(path)
            If res.genMode = GenSettings.genModes.simple Then
                res.common_settMap = Map.SettingsMap.Read(path)
                Dim lSettings() As Map.SettingsLoc = Map.SettingsLoc.Read(path, 1)
                If lSettings.Length < 2 Then Throw New Exception("Unexpected file content for simple mode")
                res.simple_settRaceLoc = lSettings(0)
                res.simple_settCommLoc = lSettings(1)
            ElseIf res.genMode = GenSettings.genModes.template Then
                res.common_settMap = Map.SettingsMap.Read(path)
                Dim lSettingsMin() As Map.SettingsLoc = Map.SettingsLoc.Read(path, 1)
                Dim lSettingsMax() As Map.SettingsLoc = Map.SettingsLoc.Read(path, 2)
                If lSettingsMin.Length < 2 Then Throw New Exception("Unexpected file content for template mode")
                ReDim res.template_settGenLoc(UBound(lSettingsMin))
                For i As Integer = 0 To UBound(lSettingsMin) Step 1
                    res.template_settGenLoc(i).minValues = lSettingsMin(i)
                    res.template_settGenLoc(i).maxValues = lSettingsMax(i)
                Next i
                Call LocationGenSetting.ReadPos(path, res.template_settGenLoc)
            Else
                Throw New Exception("Unknown reading mode")
            End If
            Return res
        End Function
        Private Sub ReadMode(ByRef path As String)
            Dim data As Dictionary(Of String, String) = ReadRawData(path)
            Call Map.ReadValue("genMode", genMode, data, GenDefaultValues.wTemplate_LocationKeyword)
        End Sub
        Public Shared Function ReadRawData(ByRef baseFilePath As String, Optional ByRef txt() As String = Nothing) As Dictionary(Of String, String)
            If IsNothing(txt) Then txt = ValueConverter.TxtSplit(IO.File.ReadAllText(baseFilePath))
            Return Map.ReadBlock(txt, GenDefaultValues.wTemplate_CreationKeyword, 1, 1, True, baseFilePath, True)
        End Function

        Public Shared Function Copy(ByRef v As GenSettings) As GenSettings
            Dim r As New GenSettings
            r.genMode = v.genMode
            r.common_settMap = Map.SettingsMap.Copy(v.common_settMap)
            r.simple_settRaceLoc = Map.SettingsLoc.Copy(v.simple_settRaceLoc)
            r.simple_settCommLoc = Map.SettingsLoc.Copy(v.simple_settCommLoc)
            r.template_settLoc = Map.SettingsLoc.Copy(v.template_settLoc)
            r.template_settGenLoc = LocationGenSetting.Copy(v.template_settGenLoc)
            Return r
        End Function

        Friend Shared Function Print(ByRef name As String, ByRef v As String) As String
            Return name & vbTab & v
        End Function
        Friend Shared Function Print(ByRef name As String, ByRef v As Integer) As String
            Return Print(name, v.ToString)
        End Function
        Friend Shared Function Print(ByRef name As String, ByRef v As Double) As String
            Return Print(name, v.ToString)
        End Function
        Friend Shared Function Print(ByRef name As String, ByRef v As Boolean) As String
            Return Print(name, v.ToString)
        End Function
        Friend Shared Function Print(ByRef name As String, ByRef v() As String, ByRef delimiter As Char) As String
            Dim r As String = ""
            If IsNothing(v) Then
                r = "Nothing"
            Else
                r = v(0)
                For i As Integer = 1 To UBound(v) Step 1
                    r &= delimiter & v(i)
                Next i
            End If
            Return Print(name, r)
        End Function
        Friend Shared Function Print(ByRef name As String, ByRef value() As Map.SettingsLoc.SettingsRaceCity, _
                                     ByRef delimiter As Char) As String
            If IsNothing(value) Then
                Return "Nothing"
            Else
                Dim res As String = ""
                For i As Integer = 0 To UBound(value) Step 1
                    res &= Map.SettingsLoc.SettingsRaceCity.Print(value(i))
                    If i < UBound(value) Then res &= delimiter
                Next i
                Return res
            End If
        End Function
        Friend Shared Function Print(ByRef valueField As ClassFieldsHandler.GetFieldResult, _
                                     ByRef delimiter As Char) As String
            If valueField.searchResultField.FieldType.FullName = GetType(String).FullName Then
                Return Print(valueField.searchResultField.Name, CType(valueField.searchResult, String))
            ElseIf valueField.searchResultField.FieldType.FullName = GetType(String()).FullName Then
                Return Print(valueField.searchResultField.Name, CType(valueField.searchResult, String()), delimiter)
            ElseIf valueField.searchResultField.FieldType.FullName = GetType(Double).FullName Then
                Return Print(valueField.searchResultField.Name, CType(valueField.searchResult, Double))
            ElseIf valueField.searchResultField.FieldType.FullName = GetType(Integer).FullName Then
                Return Print(valueField.searchResultField.Name, CType(valueField.searchResult, Integer))
            ElseIf valueField.searchResultField.FieldType.FullName = GetType(Boolean).FullName Then
                Return Print(valueField.searchResultField.Name, CType(valueField.searchResult, Boolean))
            ElseIf valueField.searchResultField.FieldType.FullName = GetType(Map.SettingsLoc.SettingsRaceCity()).FullName Then
                Return Print(valueField.searchResultField.Name, CType(valueField.searchResult, Map.SettingsLoc.SettingsRaceCity()), delimiter)
            Else
                Throw New Exception("Unexpected variable type")
                Return ""
            End If
        End Function

    End Structure

    Public Sub New(ByRef c As Common)
        comm = c
        stackLocGen = New StackLocationsGen(comm, Me)
        minLocationRadiusAtAll = c.defValues.minLocationRadiusAtAll

        ActiveObjects = New AttendedObject() {Nothing, _
                                              New AttendedObject(5, DefMapObjects.Types.Capital), _
                                              New AttendedObject(4, DefMapObjects.Types.City), _
                                              New AttendedObject(3, DefMapObjects.Types.Vendor, True), _
                                              New AttendedObject(3, DefMapObjects.Types.Mercenary, True), _
                                              New AttendedObject(3, DefMapObjects.Types.Mage, True), _
                                              New AttendedObject(3, DefMapObjects.Types.Trainer, True), _
                                              New AttendedObject(3, DefMapObjects.Types.Ruins), _
                                              New AttendedObject(1, DefMapObjects.Types.Mine)}
    End Sub

    Private Function ActiveObjectsSet(ByRef settMap As Map.SettingsMap, ByRef symmId As Integer) As Map.Cell()(,)
        Dim symm As New SymmetryOperations
        Dim result(UBound(ActiveObjects))(,) As Map.Cell
        For i As Integer = 1 To UBound(ActiveObjects) Step 1
            Dim r As Integer = ActiveObjects(i).Size + 2 * ActiveObjects(i).dxy - 1
            Dim d As Integer = ActiveObjects(i).dxy
            ReDim result(i)(r, r)

            For y As Integer = d To r - d Step 1
                For x As Integer = d To r - d Step 1
                    result(i)(x, y).passability.isAttended = True
                Next x
            Next y

            For y As Integer = r - d - 1 To r - d + 1 Step 1
                For x As Integer = r - d - 1 To r - d + 1 Step 1
                    If Not result(i)(x, y).passability.isAttended Then
                        result(i)(x, y).passability.isPenetrable = True
                    End If
                Next x
            Next y
            If ActiveObjects(i).hasExternalGuard Then
                For y As Integer = r - d To r - d + 2 Step 1
                    For x As Integer = r - d To r - d + 2 Step 1
                        If Not result(i)(x, y).passability.isAttended Then
                            result(i)(x, y).passability.isPenetrable = True
                        End If
                    Next x
                Next y
                For k As Integer = -1 To 1 Step 2
                    result(i)(r - d + k, r - d - k).passability.isPenetrable = False
                    result(i)(r - d + k, r - d - k).passability.isBorder = True
                Next k
                result(i)(r - d + 1, r - d + 1).stack.GuardLoc = True
            End If

            If symmId > -1 Then
                Dim p() As Point
                For y As Integer = 0 To r Step 1
                    For x As Integer = 0 To r Step 1
                        p = symm.ApplySymm(New Point(x, y), settMap.nRaces, New Map(r, r, symmId), 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            result(i)(p(k).X, p(k).Y).passability.isAttended = result(i)(x, y).passability.isAttended Or result(i)(p(k).X, p(k).Y).passability.isAttended
                            result(i)(p(k).X, p(k).Y).passability.isPenetrable = result(i)(x, y).passability.isPenetrable Or result(i)(p(k).X, p(k).Y).passability.isPenetrable
                            result(i)(p(k).X, p(k).Y).passability.isBorder = result(i)(x, y).passability.isBorder Or result(i)(p(k).X, p(k).Y).passability.isBorder
                        Next k
                    Next x
                Next y
            End If
            result(i)(d, d).mapObject.objectID = DefMapObjects.toObjType(i)
        Next i
        Return result
    End Function

    ''' <summary>Генерирует заготовку ландшафта</summary>
    ''' <param name="maxGenTime">Максимальное время на операцию расстановки объектов.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет Nothing</param>
    Public Function GenMap(ByRef settGen As GenSettings, ByRef maxGenTime As Integer, ByRef log As Log) As Map
        log.Add(vbNewLine & "-----New map creation attempt-----")
        settGen.common_settMap.xSize -= 1
        settGen.common_settMap.ySize -= 1

        If Not settGen.common_settMap.isChecked Then Throw New Exception("Check parameters via settGen.common_settMap.Check()")
        If settGen.genMode = GenSettings.genModes.simple Then
            '    If Not settGen.simple_settRaceLoc.isChecked Then Throw New Exception("Check parameters via settGen.simple_settRaceLoc.Check()")
            '    If Not settGen.simple_settCommLoc.isChecked Then Throw New Exception("Check parameters via settGen.simple_settCommLoc.Check()")
        ElseIf settGen.genMode = GenSettings.genModes.template Then
            '    For i As Integer = 0 To UBound(settGen.template_settLoc) Step 1
            '        If Not settGen.template_settLoc(i).isChecked Then Throw New Exception("Check parameters via settGen.template_settLoc(" & i & ").Check()")
            '    Next i
        Else
            Throw New Exception("Unknown gen mode")
        End If

        Dim term As New TerminationCondition(maxGenTime)
        Dim AttemptsN = 0
        Dim m As Map = Nothing
        Dim nTry As Integer = 0
        Dim logString As String = ""
        Dim selectedSymmetryClass As Integer
        Dim copiedSettings() As Map.SettingsLoc

        If Not IsNothing(log) Then
            m = New Map(-1, -1, -1) With {.log = log, .comm = comm}
        End If

        Do While AttemptsN < 5
            Try
newtry:
                copiedSettings = Nothing
                selectedSymmetryClass = SelectSymmetryClass(settGen.common_settMap)

                Dim t0 As Integer
                Dim txt As String
                If Not IsNothing(m) Then logString = m.log.PrintAll & vbNewLine & vbNewLine
                logString &= "Impenetrable mesh gen, attempt: " & AttemptsN + 1
                If nTry > 0 Then logString &= " Try: " & nTry + 1

                If settGen.genMode = GenSettings.genModes.simple Then
                    t0 = Environment.TickCount
                    m = PlaceRaceLocations(settGen.common_settMap, settGen.simple_settRaceLoc, selectedSymmetryClass, logString)
                    Call m.log.Add("Playable races locations creating: " & Environment.TickCount - t0 & " ms")

                    t0 = Environment.TickCount
                    Call PlaceCommonLocs(m, settGen.common_settMap, settGen.simple_settCommLoc)
                    copiedSettings = Map.SettingsLoc.ToArray(settGen.simple_settRaceLoc, _
                                                             settGen.simple_settCommLoc, _
                                                             settGen.common_settMap.nRaces, _
                                                             m.Loc.Length)
                    Call m.log.Add("Common locations creating: " & Environment.TickCount - t0 & " ms")
                ElseIf settGen.genMode = GenSettings.genModes.template Then
                    t0 = Environment.TickCount
                    m = PlaceLocationsByTemplate(settGen, selectedSymmetryClass, logString)
                    If IsNothing(m.board) Then GoTo clearandexit
                    copiedSettings = Map.SettingsLoc.Copy(settGen.template_settLoc)
                    Call m.log.Add("Locations creating with template: " & Environment.TickCount - t0 & " ms")
                Else
                    txt = ""
                    copiedSettings = Nothing
                End If

                t0 = Environment.TickCount
                Call SetLocIdToCells(m, settGen.common_settMap)
                Call m.log.Add("Setting location ID to tiles: " & Environment.TickCount - t0 & " ms")
                t0 = Environment.TickCount
                Call SetBorders(m, settGen.common_settMap, copiedSettings, term)
                Call stackLocGen.PlasePassesGuards(m, settGen.common_settMap, True, term)
                Call m.log.Add("Passages creating: " & Environment.TickCount - t0 & " ms")
                t0 = Environment.TickCount
                Call PlaceActiveObjects(m, settGen.common_settMap, copiedSettings, term)
                term = New TerminationCondition(term.maxTime)
                Call m.log.Add("Active objects creating: " & Environment.TickCount - t0 & " ms")
                t0 = Environment.TickCount
                Call MakeLabyrinth(m, settGen.common_settMap, copiedSettings, term)
                Call m.log.Add("Impassable tiles creating: " & Environment.TickCount - t0 & " ms")
                AttemptsN += 1
                If Not term.ExitFromLoops Then
                    m.complited.LoationsCreation_Done = True
                    Return m
                Else
                    Call m.log.Add("Impenetrable mesh gen: time limit termination")
                End If
                nTry = 0
            Catch ex As Exception
                Call m.log.Add("Some error occured in ImpenetrableMeshGen: " & vbNewLine & ex.Message & vbNewLine & ex.StackTrace)
                m.Clear()
                If nTry > 1 Then
                    Throw ex
                Else
                    nTry += 1
                End If
            End Try
        Loop
clearandexit:
        m.Clear()
        Return m
    End Function
    Private Function SelectSymmetryClass(ByRef settMap As Map.SettingsMap) As Integer
        Dim selectedSymmetryClass As Integer
        If settMap.ApplySymmetry Then
            Dim slist As List(Of Integer) = symm.PossibleOperationsList(settMap.nRaces, _
                                                                        settMap.xSize, _
                                                                        settMap.ySize)
            If settMap.SymmetryClass > -1 Then
                selectedSymmetryClass = settMap.SymmetryClass
                If Not slist.Contains(selectedSymmetryClass) Then
                    Throw New Exception("Выбранная симметрия не подходит под выбранные параметры карты (соотношение сторон и количество рас")
                End If
            Else
                If IsNothing(slist) OrElse slist.Count = 0 Then Throw New Exception("Должно быть две или четыре расы для возможности создать симметричную карту")
                selectedSymmetryClass = comm.RandomSelection(slist, True)
            End If
        Else
            selectedSymmetryClass = -1
        End If
        Return selectedSymmetryClass
    End Function

#Region "Place locations"
    Public Function PlaceLocationsByTemplate(ByRef settGen As GenSettings, ByRef symmID As Integer, ByRef previousLogText As String) As Map
        Dim res As New Map(settGen.common_settMap.xSize, settGen.common_settMap.ySize, symmID, comm)
        Call res.log.Add(previousLogText)

        Dim loc() As Location = Nothing
        Dim usedPos As New List(Of String)
        Dim attempt1, attempt2 As Integer
        Dim exitLoop1, exitLoop2 As Boolean
        Dim lsett() As Map.SettingsLoc = Nothing
        'Dim checkResult As String
        Dim invalidSettings As Boolean = False
        Dim errortext As String = "It was not possible to properly place locations in accordance with the specified template"
        Dim deltaI As Integer

        attempt2 = 0
        exitLoop2 = False
        Do While attempt2 < 10 And Not exitLoop2
            attempt2 += 1
            attempt1 = 0
            exitLoop1 = False
            Do While attempt1 < 10 And Not exitLoop1
                attempt1 += 1
                exitLoop1 = True
                deltaI = 0
                ReDim lsett(UBound(settGen.template_settGenLoc))
                For i As Integer = 0 To UBound(settGen.template_settGenLoc) Step 1
                    If TestChance(settGen.template_settGenLoc(i).AppearanceChance.RandomValue(rndgen)) Then
                        lsett(i - deltaI) = GenSettings.LocationGenSetting.RandomizeSettings( _
                                                              settGen.template_settGenLoc(i).minValues, _
                                                              settGen.template_settGenLoc(i).maxValues, _
                                                              rndgen)
                        Call lsett(i - deltaI).IncrementOwner(0, settGen.common_settMap.nRaces)
                        'checkResult = lsett(i - deltaI).Check
                        'If checkResult.Length > 0 Then
                        '    invalidSettings = True
                        '    Call res.log.Add("Invalid settings detected in settings #" & i + 1 & ":")
                        '    Call res.log.Add(checkResult)
                        'End If
                    Else
                        deltaI += 1
                    End If
                Next i
                ReDim Preserve lsett(UBound(settGen.template_settGenLoc) - deltaI)
                ReDim loc(UBound(lsett))

                If invalidSettings Then
                    res.Clear()
                    Return res
                End If

                usedPos.Clear()
                Dim x, y As Integer
                For i As Integer = 0 To UBound(lsett) Step 1
                    loc(i) = Location.GenLocSize(lsett(i), i + 1, rndgen, 1)

                    x = CInt(settGen.template_settGenLoc(i).posX.RandomValue(rndgen) * settGen.common_settMap.xSize)
                    x = Math.Max(0, Math.Min(settGen.common_settMap.xSize, x))
                    y = CInt(settGen.template_settGenLoc(i).posY.RandomValue(rndgen) * settGen.common_settMap.ySize)
                    y = Math.Max(0, Math.Min(settGen.common_settMap.ySize, y))

                    If Not usedPos.Contains(x & "_" & y) Then
                        usedPos.Add(x & "_" & y)
                        loc(i).pos = New Point(x, y)
                    Else
                        exitLoop1 = False
                    End If
                Next i
            Loop
            If Not exitLoop1 Then
                Call res.log.Add(errortext)
                res.Clear()
                Return res
            End If

            If symmID > -1 Then
                exitLoop2 = True
                Dim symmLocs(), outLocs(4 * loc.Length - 1) As Location
                Dim outSett(UBound(outLocs)) As Map.SettingsLoc
                Dim n As Integer = -1

                For i As Integer = 0 To UBound(loc) Step 1
                    Dim d As Integer = MinimalSqDistanceForLocationsObtainedBySymmetry(loc(i))
                    symmLocs = symm.ApplySymm(loc(i), settGen.common_settMap.nRaces, res, d)
                    If Not symmLocs.Length = settGen.common_settMap.nRaces Then
                        If i = 0 Or Not IsNothing(lsett(i).RaceCities) Then
                            exitLoop2 = False
                            Exit For
                        End If
                    End If
                    For j As Integer = 0 To UBound(symmLocs) Step 1
                        n += 1
                        outLocs(n) = Location.Copy(symmLocs(j))
                        outLocs(n).ID = n + 1
                        outSett(n) = Map.SettingsLoc.Copy(lsett(i))
                        Call outSett(n).IncrementOwner(j, settGen.common_settMap.nRaces)
                    Next j
                Next i
                If exitLoop2 Then
                    For i As Integer = 0 To n Step 1
                        Dim d As Integer = MinimalSqDistanceForLocationsObtainedBySymmetry(outLocs(i))
                        For j As Integer = 0 To n Step 1
                            If Not i = j Then
                                If outLocs(i).pos.SqDist(outLocs(j).pos) < d Then
                                    i = n
                                    j = n
                                    exitLoop2 = False
                                End If
                            End If
                        Next j
                    Next i
                    If exitLoop2 Then
                        ReDim Preserve outLocs(n), outSett(n)
                        loc = outLocs
                        lsett = outSett
                    End If
                End If
            Else
                exitLoop2 = True
            End If
        Loop
        If Not exitLoop2 Then
            Call res.log.Add(errortext)
            res.Clear()
            Return res
        End If

        res.Loc = loc
        settGen.template_settLoc = lsett

        Return res
    End Function
    Private Function TestChance(ByRef chance As Double) As Boolean
        If chance <= 0 Then
            Return False
        ElseIf chance >= 1 Then
            Return True
        Else
            If rndgen.Rand(0, 1) > chance Then
                Return False
            Else
                Return True
            End If
        End If
    End Function

    Private Sub PlaceLoc(ByRef m As Map, ByRef loc As Location)
        Dim b As Location.Borders = loc.XYborders(UBound(m.board, 1), UBound(m.board, 2))
        Dim plist As New List(Of Point)
        For x As Integer = b.minX To b.maxX Step 1
            For y As Integer = b.minY To b.maxY Step 1
                If loc.IsInside(x, y) Then
                    m.board(x, y).AddToLocIDArray(loc.ID)
                    plist.Add(New Point(x, y))
                End If
            Next y
        Next x
        For Each p As Point In plist
            b = NearestXY(p, m, 2)
            For x As Integer = b.minX To b.maxX Step 1
                For y As Integer = b.minY To b.maxY Step 1
                    If m.board(x, y).locID.Count = 0 Then
                        m.board(p.X, p.Y).passability.isBorder = True
                        x = b.maxX
                        Exit For
                    End If
                Next y
            Next x
        Next p
    End Sub
    Friend Shared Function NearestXY(ByRef x As Integer, ByRef y As Integer, _
                                     ByRef xSize As Integer, ByRef ySize As Integer, _
                                     ByRef tolerance As Integer) As Location.Borders
        Return New Location.Borders With {.minx = Math.Max(x - tolerance, 0), _
                                          .maxx = Math.Min(x + tolerance, xSize), _
                                          .miny = Math.Max(y - tolerance, 0), _
                                          .maxy = Math.Min(y + tolerance, ySize)}
    End Function
    Friend Shared Function NearestXY(ByRef P As Point, ByRef M As Map, ByRef tolerance As Integer) As Location.Borders
        Return NearestXY(P.X, P.Y, M.xSize, M.ySize, tolerance)
    End Function

    Private Function PlaceRaceLocations(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef symmID As Integer, ByRef previousLogText As String) As Map
        Dim res As New Map(settMap.xSize, settMap.ySize, symmID, comm)
        Call res.log.Add(previousLogText)

        Dim ok As Boolean = False
        Dim raceLocs() As Location
        Dim prepResult As PrepareToRaceLocGenResult = PrepareToRaceLocGen(settMap, settRaceLoc)
        If res.symmID > -1 Then
            Dim id As Integer
            Dim L As Location
            raceLocs = Nothing
            Do While Not ok
                L = Location.Copy(prepResult.raceLocs(0))
                id = comm.RandomSelection(prepResult.ppIDs, True)
                L.pos = New Point(prepResult.possiblePoints(id).X, prepResult.possiblePoints(id).Y)
                raceLocs = symm.ApplySymm(L, settMap.nRaces, res, prepResult.IminimumDist)
                If raceLocs.Length = settMap.nRaces Then
                    ok = TestRaceLocations(raceLocs, settMap.nRaces, prepResult, settMap.RaceLocsDistTolerance)
                Else
                    ok = False
                End If
            Loop
        Else
            Dim tryagain As Boolean = False
            Dim borderPoints() As Point = Nothing
            Dim id, t As Integer
            raceLocs = prepResult.raceLocs
            Dim SelID, tmpID As New List(Of Integer)
            Do While Not ok
                id = comm.RandomSelection(prepResult.ppIDs, True)
                raceLocs(0).pos = New Point(prepResult.possiblePoints(id).X, prepResult.possiblePoints(id).Y)

                SelID.Clear()
                For Each i As Integer In prepResult.equalDist(raceLocs(0).pos.X, raceLocs(0).pos.Y)
                    SelID.Add(i)
                Next i
                For n As Integer = 1 To settMap.nRaces - 1 Step 1
                    t = comm.RandomSelection(SelID, True)
                    raceLocs(n).pos = New Point(prepResult.possiblePoints(t).X, prepResult.possiblePoints(t).Y)

                    If n < settMap.nRaces - 1 Then
                        tmpID.Clear()
                        For Each i As Integer In SelID
                            tmpID.Add(i)
                        Next i
                        For Each i As Integer In tmpID
                            If Not prepResult.equalDist(raceLocs(n).pos.X, raceLocs(n).pos.Y).Contains(i) Then SelID.Remove(i)
                        Next i
                        If SelID.Count = 0 Then
                            tryagain = True
                            Exit For
                        End If
                    End If
                Next n
                If tryagain Then
                    tryagain = False
                Else
                    ok = TestRaceLocations(raceLocs, settMap.nRaces, prepResult, settMap.RaceLocsDistTolerance)
                End If
            Loop
        End If
        res.Loc = raceLocs
        For i As Integer = 0 To UBound(res.Loc) Step 1
            Call PlaceLoc(res, res.Loc(i))
        Next i
        Return res
    End Function
    Private Function PrepareToRaceLocGen(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc) As PrepareToRaceLocGenResult
        Dim result As New PrepareToRaceLocGenResult
        ReDim result.raceLocs(settMap.nRaces - 1)
        For i As Integer = 0 To settMap.nRaces - 1 Step 1
            result.raceLocs(i) = Location.GenLocSize(settRaceLoc, i + 1, rndgen, minLocationRadiusAtAll)
        Next i

        If Math.Max(settMap.xSize, settMap.ySize) < 2 * settRaceLoc.AverageRadius + 10 Then Throw New Exception("Слишком большой радиус начальных локаций")
        If settRaceLoc.AverageRadius < 7 Then Throw New Exception("Слишком маленький радиус начальных локаций")

        Dim r, m As Double
        Dim n As Integer
        If settMap.nRaces = 4 Then
            r = 0.5 * Math.Sqrt(((settMap.xSize - 2 * settRaceLoc.AverageRadius) ^ 2) _
                               + ((settMap.ySize - 2 * settRaceLoc.AverageRadius) ^ 2) - 2)
            r = Math.Min(r, Math.Min(settMap.xSize, settMap.ySize) - 2)
        ElseIf settMap.nRaces = 3 Then
            r = 0.5 * Math.Sqrt(3) * (Math.Min(settMap.xSize, settMap.ySize) - 2 * settRaceLoc.AverageRadius)
        ElseIf settMap.nRaces = 2 Then
            r = 0.5 * (settMap.xSize + settMap.ySize - 4 * settRaceLoc.AverageRadius)
        Else
            Throw New Exception("Неожиданное аколичество рас: " & settMap.nRaces)
            r = 2 * settRaceLoc.AverageRadius + 2
        End If
        r = Math.Min(r, Math.Min(settMap.xSize, settMap.ySize) - 2)
        r = r * (1 - 0.5 * settMap.RaceLocsDistTolerance)
        If settMap.nRaces = 4 Then
            m = CInt(0.5 * Math.Min(r, Math.Min(settMap.xSize, settMap.ySize) - 2 * settRaceLoc.AverageRadius))
        ElseIf settMap.nRaces = 3 Then
            m = CInt(0.25 * Math.Sqrt(3) * r)
        ElseIf settMap.nRaces = 2 Then
            m = CInt(r / Math.Sqrt(2))
        Else
            m = 0
        End If

        result.DminimumDist = r
        result.IminimumDist = CInt((r - 2) ^ 2)

        Dim dX As Integer = CInt(Math.Min(0.5 * settMap.xSize - settRaceLoc.AverageRadius - 1, m))
        Dim dY As Integer = CInt(Math.Min(0.5 * settMap.ySize - settRaceLoc.AverageRadius - 1, m))
        Dim x1 As Integer = CInt(0.5 * settMap.xSize - dX)
        Dim x2 As Integer = CInt(0.5 * settMap.xSize + dX)
        Dim y1 As Integer = CInt(0.5 * settMap.ySize - dY)
        Dim y2 As Integer = CInt(0.5 * settMap.ySize + dY)

        result.ppIDs = New List(Of Integer)
        ReDim result.possiblePoints((settMap.xSize + 1) * (settMap.ySize + 1) - 1)
        n = -1
        For x As Integer = 0 To settMap.xSize Step 1
            If x >= settRaceLoc.AverageRadius And x <= settMap.xSize - settRaceLoc.AverageRadius Then
                For y As Integer = 0 To settMap.ySize Step 1
                    If y >= settRaceLoc.AverageRadius And y <= settMap.ySize - settRaceLoc.AverageRadius Then
                        If Not (x > x1 And x < x2 And y > y1 And y < y2) Then
                            n += 1
                            result.possiblePoints(n) = New Point(x, y)
                            result.ppIDs.Add(n)
                        End If
                    End If
                Next y
            End If
        Next x
        ReDim Preserve result.possiblePoints(n)

        Dim borderPoints(2 * (settMap.xSize + settMap.ySize) - 1) As Point
        n = -1
        For i As Integer = 0 To settMap.xSize Step 1
            n += 1
            borderPoints(n) = New Point(i, 0)
            n += 1
            borderPoints(n) = New Point(i, settMap.ySize)
        Next i
        For i As Integer = 1 To settMap.ySize - 1 Step 1
            n += 1
            borderPoints(n) = New Point(0, i)
            n += 1
            borderPoints(n) = New Point(settMap.xSize, i)
        Next i

        Dim distToBorder(settMap.xSize, settMap.ySize) As Integer
        ReDim result.equalDist(settMap.xSize, settMap.ySize)
        Parallel.For(0, result.possiblePoints.Length, _
         Sub(i As Integer)
             Dim k As Integer = 0
             For j As Integer = 0 To UBound(borderPoints) Step 1
                 k += result.possiblePoints(i).SqDist(borderPoints(j))
             Next j
             distToBorder(result.possiblePoints(i).X, result.possiblePoints(i).Y) = k
         End Sub)
        Parallel.For(0, result.possiblePoints.Length, _
         Sub(i As Integer)
             Dim k As Integer = distToBorder(result.possiblePoints(i).X, result.possiblePoints(i).Y)
             Dim dk As Integer = CInt(0.1 * k)
             Dim t As New List(Of Integer)
             Dim d As Integer
             For j As Integer = 0 To UBound(result.possiblePoints) Step 1
                 If Not i = j AndAlso Math.Abs(k - distToBorder(result.possiblePoints(j).X, result.possiblePoints(j).Y)) <= dk Then
                     d = result.possiblePoints(i).SqDist(result.possiblePoints(j))
                     If d >= result.IminimumDist Then t.Add(j)
                 End If
             Next j
             result.equalDist(result.possiblePoints(i).X, result.possiblePoints(i).Y) = t
         End Sub)

        Return result
    End Function
    Private Function TestRaceLocations(ByRef RLocs() As Location, ByRef nRaces As Integer, _
                                       ByRef prep As PrepareToRaceLocGenResult, _
                                       ByRef RTolerance As Double) As Boolean
        Dim D, minD1, minD2 As Integer
        For i As Integer = 0 To nRaces - 1 Step 1
            minD1 = -1
            For j As Integer = 0 To nRaces - 1 Step 1
                If Not j = i Then
                    D = RLocs(i).pos.SqDist(RLocs(j).pos)
                    If D < prep.IminimumDist Then Return False
                    If minD1 > -1 Then
                        If minD1 > D Then
                            minD2 = minD1
                            minD1 = D
                        ElseIf minD2 > D Then
                            minD2 = D
                        End If
                    Else
                        minD1 = D
                        minD2 = D
                    End If
                End If
            Next j
            If minD1 * Math.Pow(1 + RTolerance, 2) < minD2 Then Return False
        Next i
        Return True
    End Function

    Private Sub PlaceCommonLocs(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settCommLoc As Map.SettingsLoc)

        Dim id As Integer = m.Loc.Length + 1
        Dim dynRadiusDispersion As Double = settCommLoc.maxRadiusDispersion
        Dim dynAverageRadius As Double = settCommLoc.AverageRadius
        Dim dynBaseRadius As Double = settCommLoc.AverageRadius
        Dim possiblePoints((m.xSize + 1) * (m.ySize + 1) - 1) As Point
        Dim IDs As New List(Of Integer)
        Dim nextloop As Boolean = True

        Dim sLocs() As Location
        Do While nextloop
            Dim loc As Location = Location.GenLocSize(settCommLoc, id, rndgen, minLocationRadiusAtAll)
            'make possible ids
            Dim b As Location.Borders = loc.XYborders(Integer.MaxValue, Integer.MaxValue, Integer.MinValue, Integer.MinValue)
            Dim add As Boolean
            Dim locPlist As New List(Of Point)
            IDs.Clear()
            For x As Integer = b.minX To b.maxX Step 1
                For y As Integer = b.minY To b.maxY Step 1
                    If loc.IsInside(x, y) Then locPlist.Add(New Point(x, y))
                Next y
            Next x
            For x As Integer = 0 To m.xSize Step 1
                For y As Integer = 0 To m.ySize Step 1
                    add = False
                    For Each p As Point In locPlist
                        Dim dx As Integer = x + p.X
                        If dx >= 0 And dx <= m.xSize Then
                            Dim dy As Integer = y + p.Y
                            If dy >= 0 And dy <= m.ySize Then
                                If m.board(dx, dy).passability.isBorder Then
                                    add = True
                                ElseIf m.board(dx, dy).locID.Count > 0 Then
                                    add = False
                                    Exit For
                                End If
                            End If
                        End If
                    Next p
                    If add Then
                        possiblePoints(IDs.Count) = New Point(x, y)
                        IDs.Add(IDs.Count)
                    End If
                Next y
            Next x
            '#################

            If IDs.Count > 0 Then
                Dim pid As Integer = comm.RandomSelection(IDs, True)
                loc.pos = New Point(possiblePoints(pid).X, possiblePoints(pid).Y)
                If m.symmID > -1 Then
                    Dim minR As Integer = MinimalSqDistanceForLocationsObtainedBySymmetry(loc)
                    sLocs = symm.ApplySymm(loc, settMap.nRaces, m, minR)
                    ReDim Preserve m.Loc(m.Loc.Length + UBound(sLocs))
                    For i As Integer = 0 To UBound(sLocs) Step 1
                        m.Loc(UBound(m.Loc) - UBound(sLocs) + i) = sLocs(i)
                        Call PlaceLoc(m, m.Loc(UBound(m.Loc) - UBound(sLocs) + i))
                    Next i
                    id = m.Loc.Length + 1
                Else
                    ReDim Preserve m.Loc(m.Loc.Length)
                    m.Loc(UBound(m.Loc)) = loc
                    Call PlaceLoc(m, m.Loc(UBound(m.Loc)))
                    id += 1
                End If
            Else
                nextloop = True
                'cange dyn parameters
                Dim newDisp As Double = 0.9 * dynRadiusDispersion
                If newDisp < 0.001 Then
                    dynRadiusDispersion = settCommLoc.maxRadiusDispersion
                    dynBaseRadius *= 0.9
                    dynAverageRadius = dynBaseRadius
                    If dynBaseRadius < 0.5 * settCommLoc.AverageRadius _
                    Or dynBaseRadius < minLocationRadiusAtAll Then nextloop = False
                Else
                    dynAverageRadius *= (1 - dynRadiusDispersion) / (1 - newDisp)
                    dynRadiusDispersion = newDisp
                End If
                '####################
            End If
        Loop
    End Sub
    Friend Function MinimalSqDistanceForLocationsObtainedBySymmetry(ByRef loc As Location) As Integer
        Return CInt((0.9 * Math.Min(loc.gASize, loc.gBSize)) ^ 2)
    End Function
#End Region

#Region "Set locations ID to tiles"
    ''' <summary>В зависимости от расположения и параметров локаций присвоит тайлам ID локаци</summary>
    ''' <param name="m">Хранилище данных о карте. К этому моменту должны быть 
    ''' присвоены значения переменным .xSize и .ySize (например, для карты 96x48 значения 95 и 47, соответственно)
    ''' присвоено значение для symmID - ID примененной операции симметрии (см. класс SymmetryOperations) (-1 - без симметрии)
    ''' инициализирован массив .Loc(from 0 to "количество локаций-1").
    ''' .Loc() - внутри инициализированные локации</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    Public Sub SetLocIdToCells(ByRef m As Map, ByVal settMap As Map.SettingsMap)

        ReDim m.board(m.xSize, m.ySize)

        Dim allPoints(m.board.Length - 1) As Point
        Dim selectedIDs() As Integer = Nothing
        Dim weight(UBound(allPoints))() As Double
        Dim weightSum(UBound(allPoints)) As Double
        Dim idlist As List(Of Integer) = New List(Of Integer)
        Dim pointID(m.xSize, m.ySize) As Integer

        Dim n As Integer = -1
        For x As Integer = 0 To m.xSize Step 1
            For y As Integer = 0 To m.ySize Step 1
                m.board(x, y).ClearLocIDArray() 'New List(Of Integer)
                m.board(x, y).groupID = 0
                m.board(x, y).passability.isAttended = False
                m.board(x, y).passability.isBorder = False
                m.board(x, y).passability.isPass = False
                m.board(x, y).mapObject.objectID = DefMapObjects.Types.None

                n += 1
                allPoints(n) = New Point(x, y)
                pointID(x, y) = n
            Next y
        Next x
        For Each Loc As Location In m.Loc
            m.board(Loc.pos.X, Loc.pos.Y).AddToLocIDArray(Loc.ID)
        Next Loc
        For i As Integer = 0 To UBound(weight) Step 1
            ReDim weight(i)(m.Loc.Length)
        Next i

        Dim selectedPoints() As Point = Nothing

        Dim enter_Loop As Boolean = True
        If m.symmID > -1 Then
            Do While idlist.Count > 0 Or enter_Loop
                enter_Loop = False
                Call makePointsList(m, idlist, allPoints, pointID, weight, weightSum, selectedPoints)
                If idlist.Count > 0 Then
                    Dim s() As Integer = selectPoint(idlist, weight, weightSum)
                    Dim selPointID As Integer = s(0)
                    Dim selLocID As Integer = s(1)
                    Dim pp() As Point = symm.ApplySymm(allPoints(selPointID), settMap.nRaces, m, 1)
                    selectedPoints = pp
                    If pp.Length > 1 Then
                        Dim pl() As Point = symm.ApplySymm(m.Loc(selLocID - 1).pos, settMap.nRaces, m, 1)
                        If pl.Length = pp.Length Then
                            For i As Integer = 0 To UBound(pp) Step 1
                                m.board(pp(i).X, pp(i).Y).AddToLocIDArray(m.Loc(Location.FindLocIDByPosition(m, pl(i))).ID)
                            Next i
                        ElseIf pl.Length = 1 Then
                            For i As Integer = 0 To UBound(pp) Step 1
                                m.board(pp(i).X, pp(i).Y).AddToLocIDArray(selLocID)
                            Next i
                        Else
                            Dim possibleLocs As New List(Of Integer)
                            Dim usedLocs As New List(Of Integer)
                            For Each p As Point In pp
                                possibleLocs.Clear()
                                Dim t As Integer = 1
                                Do While t < Math.Max(m.xSize, m.ySize)
                                    Dim b As Location.Borders = NearestXY(p.X, p.Y, m.xSize, m.ySize, t)
                                    For i As Integer = b.minX To b.maxX Step 1
                                        For j As Integer = b.minY To b.maxY Step 1
                                            If m.board(i, j).locID.Count > 0 Then
                                                Dim locID As Integer = m.board(i, j).locID(0) - 1
                                                If locID > -1 AndAlso Not usedLocs.Contains(locID) Then
                                                    For L As Integer = 0 To UBound(pl) Step 1
                                                        If Math.Abs(m.Loc(locID).pos.X - pl(L).X) < 2 _
                                                         And Math.Abs(m.Loc(locID).pos.Y - pl(L).Y) < 2 Then
                                                            possibleLocs.Add(locID)
                                                        End If
                                                    Next L
                                                End If
                                            End If
                                        Next j
                                    Next i
                                    If possibleLocs.Count = 0 Then
                                        If usedLocs.Count > 0 Then
                                            usedLocs.Clear()
                                        Else
                                            t += 1
                                        End If
                                    Else
                                        Exit Do
                                    End If
                                Loop
                                If possibleLocs.Count = 0 Then Throw New Exception("Не могу найти подходящую локацию")
                                Dim sel As Integer = comm.RandomSelection(possibleLocs, True)
                                m.board(p.X, p.Y).AddToLocIDArray(m.Loc(sel).ID)
                                usedLocs.Add(sel)
                            Next p
                        End If
                    Else
                        m.board(pp(0).X, pp(0).Y).AddToLocIDArray(selLocID)
                    End If
                End If
            Loop
        Else
            Do While idlist.Count > 0 Or enter_Loop
                enter_Loop = False
                Call makePointsList(m, idlist, allPoints, pointID, weight, weightSum, selectedPoints)
                If idlist.Count > 0 Then
                    Dim s() As Integer = selectPoint(idlist, weight, weightSum)
                    Dim selPointID As Integer = s(0)
                    Dim selLocID As Integer = s(1)
                    selectedPoints = {New Point(allPoints(selPointID).X, allPoints(selPointID).Y)}
                    m.board(allPoints(selPointID).X, allPoints(selPointID).Y).AddToLocIDArray(selLocID)
                End If
            Loop
        End If
    End Sub
    Private Sub makePointsList(ByRef m As Map, ByRef idlist As List(Of Integer), _
                               ByRef allPoints() As Point, _
                               ByRef pointID(,) As Integer, _
                               ByRef weight()() As Double, ByRef weightSum() As Double, _
                               ByRef selectedPoints() As Point)
        If Not IsNothing(selectedPoints) Then
            For Each p As Point In selectedPoints
                Dim pID As Integer = pointID(p.X, p.Y)
                If idlist.Contains(pID) Then idlist.Remove(pID)
            Next p
            For Each p As Point In selectedPoints
                Dim b As Location.Borders = NearestXY(p.X, p.Y, m.xSize, m.ySize, 1)
                For j As Integer = b.minY To b.maxY Step 1
                    For i As Integer = b.minX To b.maxX Step 1
                        Call makePointsList_handlePoint(m, idlist, i, j, allPoints, pointID, weight, weightSum)
                    Next i
                Next j
            Next p
            selectedPoints = Nothing
        Else
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    Call makePointsList_handlePoint(m, idlist, x, y, allPoints, pointID, weight, weightSum)
                Next x
            Next y
        End If
    End Sub
    Private Sub makePointsList_handlePoint(ByRef m As Map, ByRef idlist As List(Of Integer), _
                                           ByRef x As Integer, ByRef y As Integer, _
                                           ByRef allPoints() As Point, _
                                           ByRef pointID(,) As Integer, _
                                           ByRef weight()() As Double, ByRef weightSum() As Double)
        If m.board(x, y).locID.Count = 0 Then
            Dim pID As Integer = pointID(x, y)
            Dim locID As Integer
            Dim b As Location.Borders = NearestXY(x, y, m.xSize, m.ySize, 1)
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    If m.board(i, j).locID.Count > 0 Then
                        locID = m.board(i, j).locID(0)
                        If Not m.Loc(locID - 1).IsObtainedBySymmery And weight(pID)(locID) = 0 Then
                            weight(pID)(locID) = Math.Max(m.Loc(locID - 1).pWeight(allPoints(pID)), 10 ^ -9)
                            weightSum(pID) += weight(pID)(locID)
                        End If
                    End If
                Next i
            Next j
            If Not idlist.Contains(pID) Then idlist.Add(pID)
        End If
    End Sub
    Private Function selectPoint(ByRef idlist As List(Of Integer), _
                                 ByRef weight()() As Double, ByRef weightSum() As Double) As Integer()
        Dim selectedPointID As Integer = comm.RandomSelection(idlist, weightSum, True)

        Dim locs As New List(Of Integer)
        For i As Integer = 0 To UBound(weight(selectedPointID)) Step 1
            If weight(selectedPointID)(i) > 0 Then locs.Add(i)
        Next i
        Dim selectedLoc As Integer = comm.RandomSelection(locs, weight(selectedPointID), True)
        Return New Integer() {selectedPointID, selectedLoc}
    End Function
#End Region

#Region "Create location borders and passages"
    ''' <summary>Выполняется после SetLocIdToCells. В зависимости от того, какие ID локаци присвоены тайлам, расставит проходы стенки между локациями</summary>
    ''' <param name="m">Хранилище данных о карте. К этому моменту должны быть 
    ''' присвоены значения переменным .xSize и .ySize (например, для карты 96x48 значения 95 и 47, соответственно)
    ''' присвоено значение для symmID - ID примененной операции симметрии (см. класс SymmetryOperations) (-1 - без симметрии)
    ''' инициализированы массивы.
    ''' .Loc(from 0 to "количество локаций-1") и .board(from 0 to xSize, from 0 to ySize).
    ''' .Loc() - внутри инициализированные локации.
    ''' .board(,).LocID - как минимум одно значение (можно больше, если по соседству с тайлом есть тайлы других локаций (но тот, что первый в списке - основной)</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="Term">Нужно инициализировать экземпляр этого класса до начала генерации</param>
    Public Sub SetBorders(ByRef m As Map, ByVal settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc, ByRef Term As TerminationCondition)

        Dim tmpm As Map = m
        Dim del(tmpm.xSize, tmpm.ySize), freeze(tmpm.xSize, tmpm.ySize) As Boolean
        Dim LocBorders(UBound(tmpm.Loc), UBound(tmpm.Loc)) As Dictionary(Of String, Point)

        Dim borderRadius(tmpm.xSize, tmpm.ySize) As Integer
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                borderRadius(x, y) = -1
            Next x
        Next y

        ' определяем, какие тайлы могут быть границей между локациями {
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                If borderRadius(x, y) = -1 Then
                    Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                    Dim id As Integer = tmpm.board(x, y).locID(0)
                    Dim isBorder As Boolean = False
                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            If Not id = tmpm.board(i, j).locID(0) Then
                                isBorder = True
                                i = b.maxX
                                Exit For
                            End If
                        Next j
                    Next i
                    If isBorder Then
                        borderRadius(x, y) = rndgen.RndPos(2, True) - 1
                        If tmpm.symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, 1)
                            For i As Integer = 0 To UBound(p) Step 1
                                borderRadius(p(i).X, p(i).Y) = borderRadius(x, y)
                            Next i
                        End If
                    End If
                End If
            Next x
        Next y
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                If borderRadius(x, y) > -1 Then
                    Dim id As Integer = tmpm.board(x, y).locID(0)
                    freeze(x, y) = True
                    Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, borderRadius(x, y))
                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            tmpm.board(i, j).passability.isBorder = True
                            If Not tmpm.board(i, j).locID.Contains(id) Then tmpm.board(i, j).AddToLocIDArray(id)
                        Next j
                    Next i
                End If
            Next x
        Next y
        borderRadius = Nothing
        ' } закончили определять, какие тайлы могут быть границей между локациями

        ' делаем границы тоньше и рельефнее {
        Dim nNeighbours(tmpm.xSize, tmpm.ySize) As Integer
        Dim nRepeatLoop As Integer = 2
        Dim nRepeatLoopSmall As Integer = 3
        For repeatloop As Integer = 0 To Math.Max(nRepeatLoop, nRepeatLoopSmall) Step 1
            Parallel.For(0, tmpm.ySize + 1, _
             Sub(y As Integer)
                 Dim n As Integer
                 Dim b As Location.Borders
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If Not freeze(x, y) Then
                         n = 0
                         b = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                         For i As Integer = b.minX To b.maxX Step 1
                             For j As Integer = b.minY To b.maxY Step 1
                                 If Not tmpm.board(i, j).passability.isBorder Then
                                     n += 1
                                 End If
                             Next j
                         Next i
                         nNeighbours(x, y) = n
                     End If
                 Next x
             End Sub)
            For y As Integer = 0 To tmpm.ySize Step 1
                For x As Integer = 0 To tmpm.xSize Step 1
                    Dim averageR As Double = 0.5 * (tmpm.Loc(tmpm.board(x, y).locID(0) - 1).gASize _
                                                  + tmpm.Loc(tmpm.board(x, y).locID(0) - 1).gBSize)
                    If repeatloop <= nRepeatLoop Or averageR <= comm.defValues.smallLocationRadius Then
                        If nNeighbours(x, y) > 1 Then
                            Dim r As Integer = rndgen.RndPos(5, False)
                            If Math.Abs(nNeighbours(x, y) - 5) > r Then del(x, y) = True
                            nNeighbours(x, y) = 0
                            If tmpm.symmID > -1 Then
                                Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, 1)
                                For i As Integer = 0 To UBound(p) Step 1
                                    nNeighbours(p(i).X, p(i).Y) = nNeighbours(x, y)
                                    del(p(i).X, p(i).Y) = del(x, y)
                                Next i
                            End If
                        ElseIf nNeighbours(x, y) = 1 Then
                            nNeighbours(x, y) = 0
                        End If
                    End If
                Next x
            Next y
            Parallel.For(0, tmpm.ySize + 1, _
             Sub(y As Integer)
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If del(x, y) Then
                         tmpm.board(x, y).passability.isBorder = False
                         del(x, y) = False
                     End If
                 Next x
             End Sub)
        Next repeatloop
        freeze = Nothing
        nNeighbours = Nothing
        ' } закончили делать границы тоньше 

        ' для соседствующих между собой локаций составляем списки пограничных тайлов {
        For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
            For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                LocBorders(i, j) = New Dictionary(Of String, Point)
            Next j
        Next i
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                If tmpm.board(x, y).passability.isBorder Then
                    Dim id As Integer = tmpm.board(x, y).locID(0)
                    Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            Dim n As Integer = tmpm.board(i, j).locID(0)
                            If Not id = n Then
                                Dim s As String = i & "_" & j
                                Dim LID1 As Integer = Math.Min(id, n) - 1
                                Dim LID2 As Integer = Math.Max(id, n) - 1
                                If Not LocBorders(LID1, LID2).ContainsKey(s) Then LocBorders(LID1, LID2).Add(s, New Point(i, j))
                            End If
                        Next j
                    Next i
                End If
            Next x
        Next y
        For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
            For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                If LocBorders(i, j).Count < settMap.minPassWidth + 0.5 * settMap.minPassDist Then LocBorders(i, j).Clear()
            Next j
        Next i
        ' } закончили составлять списки пограничных тайлов

        ' убираем из возможных тайлов для создания прохода между локациямите, что находится слишком близко к какой-либо третьей локации
        Dim delList As New List(Of String)
        Dim nearRLocs As New List(Of Integer)
        For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
            For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                Dim dR As Double
                If i < settMap.nRaces Then
                    dR = 2 * (settMap.minPassDist + settMap.minPassWidth + 1)
                Else
                    dR = 1.2 * (settMap.minPassDist + settMap.minPassWidth) + 1
                End If
                Do While dR >= 1
                    delList.Clear()
                    For Each k As String In LocBorders(i, j).Keys
                        Dim p As Point = LocBorders(i, j).Item(k)
                        Dim minD As Double = 2 * settMap.minPassWidth + 1 + dR
                        Dim b As Location.Borders = NearestXY(p.X, p.Y, tmpm.xSize, tmpm.ySize, CInt(Math.Ceiling(minD)))
                        minD *= minD
                        nearRLocs.Clear()
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                Dim id As Integer = tmpm.board(x, y).locID(0) - 1
                                If (i >= settMap.nRaces And Not id = i And Not id = j AndAlso Not nearRLocs.Contains(id)) _
                                OrElse (i < settMap.nRaces And id < settMap.nRaces AndAlso Not nearRLocs.Contains(id)) Then
                                    If CDbl(p.SqDist(x, y)) <= minD Then
                                        nearRLocs.Add(id)
                                        If nearRLocs.Count > 1 Then
                                            x = b.maxX
                                            y = b.maxY
                                        End If
                                    End If
                                End If
                            Next y
                        Next x
                        If nearRLocs.Count > 1 Then delList.Add(k)
                    Next k
                    If delList.Count > 0 Then
                        If delList.Count = LocBorders(i, j).Count Then
                            dR *= 0.9
                        Else
                            dR = 0
                        End If
                    Else
                        dR = 0
                    End If
                Loop
                If delList.Count > 0 Then
                    If delList.Count = LocBorders(i, j).Count Then
                        Dim xsum, ysum, minD, D As Integer
                        Dim k As String = ""
                        xsum = 0
                        ysum = 0
                        minD = Integer.MaxValue
                        For Each p As Point In LocBorders(i, j).Values
                            xsum += p.X
                            ysum += p.Y
                        Next p
                        xsum = CInt(xsum / LocBorders(i, j).Count)
                        ysum = CInt(ysum / LocBorders(i, j).Count)
                        For Each p As Point In LocBorders(i, j).Values
                            D = p.SqDist(xsum, ysum)
                            If D < minD Or (D = minD AndAlso rndgen.PRand(0, 1) > 0.5) Then
                                minD = D
                                k = p.X & "_" & p.Y
                            End If
                        Next p
                        delList.Remove(k)
                    End If
                    For Each k As String In delList
                        LocBorders(i, j).Remove(k)
                    Next k
                End If
            Next j
        Next i
        ' } закончили убирать лишние тайлы

        ' создаем список неэквивалентных пар локаций, чтобы не создать проходы дважды {
        Dim equalLocPairsList As New List(Of String)
        If tmpm.symmID > -1 Then
            Dim Id(1)(), a() As Integer
            Dim freezed As New List(Of String)
            For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
                For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                    If LocBorders(i, j).Count > 0 Then
                        freezed.Add(i & "_" & j)
                        a = New Integer() {i, j}
                        Parallel.For(0, 2, _
                         Sub(k As Integer)
                             Dim p() As Point = symm.ApplySymm(tmpm.Loc(a(k)).pos, settMap.nRaces, tmpm, 1)
                             ReDim Id(k)(UBound(p))
                             For q As Integer = 0 To UBound(p) Step 1
                                 Id(k)(q) = Location.FindLocIDByPosition(tmpm, p(q))
                             Next q
                         End Sub)
                        For r As Integer = 0 To UBound(Id(0)) Step 1
                            For t As Integer = 0 To UBound(Id(1)) Step 1
                                If Not Id(0)(r) = Id(1)(t) Then
                                    Dim LID1 As Integer = Math.Min(Id(0)(r), Id(1)(t))
                                    Dim LID2 As Integer = Math.Max(Id(0)(r), Id(1)(t))
                                    Dim s As String = LID1 & "_" & LID2
                                    If Not equalLocPairsList.Contains(s) AndAlso Not freezed.Contains(s) Then equalLocPairsList.Add(s)
                                End If
                            Next t
                        Next r
                    End If
                Next j
            Next i
        End If
        ' } закончили делать список

        Parallel.For(0, UBound(tmpm.Loc), _
         Sub(i As Integer)
             Dim ids As New List(Of Integer)
             Dim selected, delete As New List(Of Integer)
             Dim startI As Integer = i + 1
             If i < settMap.nRaces Then
                 For j As Integer = settMap.nRaces To UBound(tmpm.Loc) Step 1
                     If LocBorders(i, j).Count > 0 Then
                         startI = settMap.nRaces
                         Exit For
                     End If
                 Next j
             End If
             For j As Integer = startI To UBound(tmpm.Loc) Step 1
                 If LocBorders(i, j).Count > 0 AndAlso Not equalLocPairsList.Contains(i & "_" & j) Then
                     ' выбор точек, через которые пройдут проходы между локациями {
                     Dim maxD, D As Integer
                     Dim pointsslist(LocBorders(i, j).Count - 1) As Point
                     LocBorders(i, j).Values.CopyTo(pointsslist, 0)
                     maxD = 0
                     ids.Clear()
                     selected.Clear()
                     delete.Clear()
                     For p1 As Integer = 0 To UBound(pointsslist) - 1 Step 1
                         ids.Add(p1)
                         For p2 As Integer = p1 + 1 To UBound(pointsslist) Step 1
                             D = pointsslist(p1).SqDist(pointsslist(p2))
                             If maxD < D Then maxD = D
                         Next p2
                     Next p1
                     ids.Add(UBound(pointsslist))
                     Dim maxPaths As Integer = CInt(Math.Max(Math.Sqrt(maxD) / settMap.minPassDist, 1))
                     For k As Integer = 1 To maxPaths Step 1
                         Dim s As Integer = comm.RandomSelection(ids, False)
                         selected.Add(s)
                         ids.Remove(s)
                         For Each p As Integer In ids
                             D = pointsslist(s).SqDist(pointsslist(p))
                             If D < settMap.minPassDist * settMap.minPassDist Then delete.Add(p)
                         Next p
                         For Each p As Integer In delete
                             ids.Remove(p)
                         Next p
                         If ids.Count = 0 Then Exit For
                     Next k
                     ' } закончили выбор точек
                     Dim Centers() As Point = New Point() {tmpm.Loc(i).pos, tmpm.Loc(j).pos}
                     For Each c As Point In Centers
                         For Each p As Integer In selected
                             Call MakePass(tmpm, pointsslist(p), c, settMap, True, False)
                         Next p
                     Next c
                 End If
             Next j
         End Sub)

        Term = New TerminationCondition(Term.maxTime)
        Call ConnectDisconnectedAreas(tmpm, settMap, Term)
        If Term.ExitFromLoops Then Exit Sub

        Call DisconnectRandomLocations(tmpm, settMap, settLoc)

        Call RecheckPassStatus(tmpm, settMap)

        Parallel.For(0, tmpm.ySize + 1, _
         Sub(y As Integer)
             For x As Integer = 0 To tmpm.xSize Step 1
                 If Not tmpm.board(x, y).passability.isBorder And tmpm.board(x, y).locID.Count > 1 Then
                     Dim n As Integer = tmpm.board(x, y).locID(0)
                     tmpm.board(x, y).ClearLocIDArray()
                     tmpm.board(x, y).AddToLocIDArray(n)
                 End If
             Next x
         End Sub)

        m = tmpm
    End Sub
    Protected Friend Shared Sub RecheckPassStatus(ByRef m As Map, ByVal settMap As Map.SettingsMap)
        Dim tmpm As Map = m
        Parallel.For(0, tmpm.ySize + 1, _
         Sub(y As Integer)
             For x As Integer = 0 To tmpm.xSize Step 1
                 If tmpm.board(x, y).passability.isPass Then
                     If settMap.AddGuardsBetweenLocations Then
                         Dim removePathStatus As Boolean = True
                         Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                         For j As Integer = b.minY To b.maxY Step 1
                             For i As Integer = b.minX To b.maxX Step 1
                                 If Not tmpm.board(x, y).locID(0) = tmpm.board(i, j).locID(0) And Not tmpm.board(i, j).passability.isBorder Then
                                     removePathStatus = False
                                     i = b.maxX
                                     j = b.maxY
                                 End If
                             Next i
                         Next j
                         tmpm.board(x, y).passability.isPass = Not removePathStatus
                     Else
                         tmpm.board(x, y).passability.isPass = False
                     End If
                 End If
             Next x
         End Sub)
        m = tmpm
    End Sub
    Private Sub ConnectDisconnectedAreas(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                         ByRef Term As TerminationCondition)
        Dim conn2(0, 0) As Boolean
        Do While Not IsNothing(conn2)
            Term.CheckTime()
            If Term.ExitFromLoops Then Exit Sub
            conn2 = Nothing
            Dim init As Point = Nothing
            For x As Integer = 0 To m.xSize Step 1
                For y As Integer = 0 To m.ySize Step 1
                    If Not m.board(x, y).passability.isBorder Then
                        init = New Point(x, y)
                        Exit For
                    End If
                Next y
                If Not IsNothing(init) Then Exit For
            Next x
            Dim conn1(,) As Boolean = FindConnected(m, init)
            Dim dp As Point = FindDisconnected(m, conn1)
            If Not IsNothing(dp) Then
                conn2 = FindConnected(m, dp)
                Dim minD As Integer = Integer.MaxValue
                Dim D As Integer
                Dim p1, p2 As Point
                p1 = Nothing : p2 = Nothing
                For y1 As Integer = 0 To m.ySize Step 1
                    For x1 As Integer = 0 To m.xSize Step 1
                        If conn1(x1, y1) Then
                            For y2 As Integer = 0 To m.ySize Step 1
                                For x2 As Integer = 0 To m.xSize Step 1
                                    If conn2(x2, y2) Then
                                        D = New Point(x1, y1).SqDist(x2, y2)
                                        If minD > D Then
                                            minD = D
                                            p1 = New Point(x1, y1)
                                            p2 = New Point(x2, y2)
                                        End If
                                    End If
                                Next x2
                            Next y2
                        End If
                    Next x1
                Next y1
                Call MakePass(m, p1, p2, settMap, False, True)
            End If
        Loop
    End Sub
    Private Sub MakePass(ByRef m As Map, ByRef init As Point, ByRef dest As Point, _
                         ByRef settMap As Map.SettingsMap, ByRef makeIntermediatePoint As Boolean, ByRef serial As Boolean)

        Dim intermediate As Point
        If makeIntermediatePoint Then
            Dim dist As Double = init.Dist(dest)
            Dim r As Location.Borders
            r = NearestXY(init, m, CInt(Math.Ceiling(dist)))
            Dim minDist As Double = Double.MaxValue
            Dim p As New Point(-1, -1)
            For y As Integer = r.minY To r.maxY Step 1
                For x As Integer = r.minX To r.maxX Step 1
                    If m.board(x, y).locID(0) = m.board(dest.X, dest.Y).locID(0) And Not m.board(x, y).passability.isBorder Then
                        Dim d As Double = init.SqDist(x, y)
                        If minDist > d Then
                            minDist = d
                            p.X = x
                            p.Y = y
                        End If
                    End If
                Next x
            Next y
            Dim newX As Integer = -1
            Dim newY As Integer = -1
            Do While newX < 0 Or newY < 0 Or newX > m.xSize Or newY > m.ySize OrElse Not m.board(newX, newY).locID(0) = m.board(dest.X, dest.Y).locID(0)
                newX = rndgen.RndInt(p.X - 1, p.X + 1, serial)
                newY = rndgen.RndInt(p.Y - 1, p.Y + 1, serial)
            Loop
            intermediate = New Point(newX, newY)
            Call MakePass(m, init, intermediate, settMap, False, serial)
        Else
            intermediate = init
        End If

        Dim vx As Double = dest.X - intermediate.X
        Dim vy As Double = dest.Y - intermediate.Y
        Dim n As Integer = CInt(10 * Math.Max((vx * vx + vy * vy), 1))
        vx /= n
        vy /= n
        n += 10
        Dim tx, ty As Double
        Dim b As Location.Borders
        Dim setAsPath As Boolean
        For r As Integer = 0 To n Step 1
            tx = intermediate.X + CDbl(r) * vx
            ty = intermediate.Y + CDbl(r) * vy
            b = NearestXY(CInt(tx), CInt(ty), m.xSize, m.ySize, CInt(2 * (0.5 * settMap.minPassWidth + 1)))
            For y As Integer = b.minY To b.maxY Step 1
                For x As Integer = b.minX To b.maxX Step 1
                    If m.board(x, y).passability.isBorder Then
                        Dim dist As Double = Math.Sqrt(CDbl(x - tx) ^ 2 + CDbl(y - ty) ^ 2)
                        If dist < 0.5 * settMap.minPassWidth OrElse (dist <= settMap.minPassWidth AndAlso rndgen.Rand(0, 1, serial) > 0.5) Then
                            Dim c1 As Integer = Math.Max(Math.Min(x, m.xSize - 1), 1)
                            Dim c2 As Integer = Math.Max(Math.Min(y, m.ySize - 1), 1)
                            setAsPath = False
                            Dim b2 As Location.Borders = NearestXY(c1, c2, m.xSize, m.ySize, 1)
                            For j As Integer = b2.minY To b2.maxY Step 1
                                For i As Integer = b2.minX To b2.maxX Step 1
                                    If Not m.board(c1, c2).locID(0) = m.board(i, j).locID(0) Then
                                        setAsPath = True
                                        i = b2.maxX
                                        j = b2.maxY
                                    End If
                                Next i
                            Next j
                            Dim p() As Point
                            If m.symmID > -1 Then
                                p = symm.ApplySymm(New Point(c1, c2), settMap.nRaces, m, 1)
                            Else
                                p = New Point() {New Point(c1, c2)}
                            End If
                            For Each item As Point In p
                                If m.board(item.X, item.Y).passability.isBorder Then
                                    m.board(item.X, item.Y).passability.isBorder = False
                                    m.board(item.X, item.Y).passability.isPass = setAsPath
                                ElseIf m.board(item.X, item.Y).passability.isAttended Then
                                    Dim tb As Location.Borders = NearestXY(item, m, 1)
                                    For yy As Integer = tb.minY To tb.maxY Step 1
                                        For xx As Integer = tb.minX To tb.maxX Step 1
                                            If m.board(xx, yy).passability.isBorder Then
                                                m.board(xx, yy).passability.isBorder = False
                                                m.board(xx, yy).passability.isPass = setAsPath
                                            End If
                                        Next xx
                                    Next yy
                                End If
                            Next item
                        End If
                    End If
                Next x
            Next y
        Next r
    End Sub
    Friend Function FindConnected(ByRef m As Map, ByRef init As Point) As Boolean(,)
        If m.board(init.X, init.Y).passability.isBorder Then Throw New Exception("Find connected function: начальная точка непроходима")
        Dim connected(m.xSize, m.ySize), check(m.xSize, m.ySize) As Boolean
        check(init.X, init.Y) = True
        connected(init.X, init.Y) = True
        Dim r As Integer = 1
        Do While r > 0
            For j As Integer = 0 To m.ySize Step 1
                For i As Integer = 0 To m.xSize Step 1
                    If check(i, j) Then
                        Dim b As Location.Borders = NearestXY(i, j, m.xSize, m.ySize, 1)
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                If Not m.board(x, y).passability.isBorder And Not connected(x, y) Then
                                    connected(x, y) = True
                                    check(x, y) = True
                                    r += 1
                                End If
                            Next y
                        Next x
                        check(i, j) = False
                        r -= 1
                    End If
                Next i
            Next j
        Loop
        Return connected
    End Function
    Friend Function FindConnected(ByRef free(,) As Boolean, ByRef init As Point) As Boolean(,)
        Dim m As Map = FreeToMap(free)
        Return FindConnected(m, init)
    End Function
    Private Function FreeToMap(ByRef free(,) As Boolean) As Map
        Dim m As New Map(UBound(free, 1), UBound(free, 2), -1)
        For j As Integer = 0 To m.ySize Step 1
            For i As Integer = 0 To m.xSize Step 1
                m.board(i, j).passability.isBorder = Not free(i, j)
            Next i
        Next j
        Return m
    End Function
    Private Function FindDisconnected(ByRef m As Map, ByRef connected(,) As Boolean) As Point
        If Not IsNothing(connected) AndAlso connected.Length > 0 Then
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If Not m.board(x, y).passability.isBorder And Not connected(x, y) Then Return New Point(x, y)
                Next x
            Next y
        Else
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If Not m.board(x, y).passability.isBorder Then Return New Point(x, y)
                Next x
            Next y
        End If
        Return Nothing
    End Function
    Friend Function FindDisconnected(ByRef free(,) As Boolean, ByRef connected(,) As Boolean) As Point
        Dim m As Map = FreeToMap(free)
        Return FindDisconnected(m, connected)
    End Function
    Friend Function FindDisconnected(ByRef free(,) As Boolean, ByRef connected()(,) As Boolean) As Point
        If IsNothing(connected) Then Return FindDisconnected(free, New Boolean(,) {})
        Dim c(UBound(connected(0), 1), UBound(connected(0), 2)) As Boolean
        For i As Integer = 0 To UBound(connected) Step 1
            For y As Integer = 0 To UBound(c, 2) Step 1
                For x As Integer = 0 To UBound(c, 1) Step 1
                    c(x, y) = c(x, y) Or connected(i)(x, y)
                Next x
            Next y
        Next i
        Return FindDisconnected(free, c)
    End Function
    Private Sub DisconnectRandomLocations(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc)

        Dim init As Point = Nothing
        For x As Integer = 0 To m.xSize Step 1
            For y As Integer = 0 To m.ySize Step 1
                If Not m.board(x, y).passability.isBorder Then
                    init = New Point(x, y)
                    Dim b As Location.Borders = NearestXY(x, y, m.xSize, m.ySize, 1)

                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            If Not m.board(x, y).locID(0) = m.board(i, j).locID(0) Then
                                init = Nothing
                                i = b.maxX
                                j = b.maxY
                            End If
                        Next j
                    Next i
                    If Not IsNothing(init) Then
                        x = m.xSize
                        y = m.ySize
                    End If
                End If
            Next y
        Next x

        Dim borders(UBound(m.Loc), UBound(m.Loc)) As List(Of Point)
        For j As Integer = 0 To UBound(m.Loc) Step 1
            For i As Integer = 0 To UBound(m.Loc) Step 1
                borders(i, j) = New List(Of Point)
            Next i
        Next j
        Dim addedTo As New List(Of Integer)
        Dim d1, d2 As Integer
        For x As Integer = 0 To m.xSize Step 1
            For y As Integer = 0 To m.ySize Step 1
                If Not m.board(x, y).passability.isBorder Then
                    If Not settLoc(m.board(x, y).locID(0) - 1).ConnectWithAllNeighboringLocations Then
                        If Not m.Loc(m.board(x, y).locID(0) - 1).IsObtainedBySymmery Then
                            addedTo.Clear()
                            Dim b As Location.Borders = NearestXY(x, y, m.xSize, m.ySize, 1)
                            For i As Integer = b.minX To b.maxX Step 1
                                For j As Integer = b.minY To b.maxY Step 1
                                    If Not settLoc(m.board(i, j).locID(0) - 1).ConnectWithAllNeighboringLocations Then
                                        If Not m.board(x, y).locID(0) = m.board(i, j).locID(0) Then
                                            If Not addedTo.Contains(m.board(i, j).locID(0)) Then
                                                If m.board(x, y).locID(0) < m.board(i, j).locID(0) Then
                                                    d1 = m.board(x, y).locID(0)
                                                    d2 = m.board(i, j).locID(0)
                                                Else
                                                    d1 = m.board(i, j).locID(0)
                                                    d2 = m.board(x, y).locID(0)
                                                End If
                                                addedTo.Add(m.board(i, j).locID(0))
                                                borders(d1 - 1, d2 - 1).Add(New Point(x, y))
                                            End If
                                        End If
                                    End If
                                Next j
                            Next i
                        End If
                    End If
                End If
            Next y
        Next x

        Dim nonEmpty(borders.Length - 1)() As Integer
        Dim IDs As New List(Of Integer)
        Dim n As Integer = -1
        For j As Integer = 0 To UBound(m.Loc) Step 1
            For i As Integer = 0 To UBound(m.Loc) Step 1
                If borders(i, j).Count > 0 Then
                    n += 1
                    nonEmpty(n) = {i, j}
                    IDs.Add(n)
                End If
            Next i
        Next j
        If n = -1 Then Exit Sub
        Dim tmpM As New Map(m.xSize, m.ySize, m.symmID)
        Dim s() As Point
        For i As Integer = 0 To n Step 1
            Dim selected As Integer = comm.RandomSelection(IDs, True)
            IDs.Remove(selected)
            If Not TestChance(settMap.PassageCreationChance) Then
                For x As Integer = 0 To m.xSize Step 1
                    For y As Integer = 0 To m.ySize Step 1
                        tmpM.board(x, y).passability.isBorder = m.board(x, y).passability.isBorder
                    Next y
                Next x
                For Each p As Point In borders(nonEmpty(selected)(0), nonEmpty(selected)(1))
                    If m.symmID > -1 Then
                        s = symm.ApplySymm(p, settMap.nRaces, m, 1)
                        For Each item As Point In s
                            tmpM.board(item.X, item.Y).passability.isBorder = True
                        Next item
                    Else
                        tmpM.board(p.X, p.Y).passability.isBorder = True
                    End If
                Next p
                If IsNothing(FindDisconnected(tmpM, FindConnected(tmpM, init))) Then
                    For Each p As Point In borders(nonEmpty(selected)(0), nonEmpty(selected)(1))
                        If m.symmID > -1 Then
                            s = symm.ApplySymm(p, settMap.nRaces, m, 1)
                            For Each item As Point In s
                                m.board(item.X, item.Y).passability.isBorder = True
                                m.board(item.X, item.Y).passability.isPass = False
                            Next item
                        Else
                            m.board(p.X, p.Y).passability.isBorder = True
                            m.board(p.X, p.Y).passability.isPass = False
                        End If
                    Next p
                End If
            End If
        Next i

    End Sub
#End Region

#Region "Place active objects: capitals, towns, ruins, mines and merchants"
    Class ActiveObjectsPlacer

        Private ReadOnly ActiveObjects() As AttendedObject
        Private ReadOnly rndgen As RndValueGen
        Private ReadOnly LocCenter, PreferedCapitalPosition As Point
        Private ReadOnly placingObjects() As ObjectPlacingSettings
        Private Term As TerminationCondition
        Public bestOutput() As Point
        Private output() As Short
        Private joinedOutput() As Integer

        Public maxN As Integer = 0

        Private checkedVariants As New List(Of String)

        Public debug_checked As Integer
        Public debug_discarded As Integer

        Private distance(,) As Double
        Private mayPlaceIfClean()(,) As Boolean
        Private weightLayer()()(,) As Double

        Private currentWeight()() As Double

        Private free_initial_points() As Point
        Private free_initial_point_id(,) As Integer

        Private ReadOnly rWeightMultiplier() As Double = New Double() {0.1, 0.15}

        'делаем битовый массив длиной кол-во свободных точек X кол-во вариантов площади
        ' по площади и номеру тайла ставим 1 в нужный слот массива, потом делаем из него строку, возвращаем в исходное положение

        Public Sub New(ByRef ao() As AttendedObject, ByRef lc As Point, ByRef pcp As Point, _
                       ByRef po() As ObjectPlacingSettings, ByRef t As TerminationCondition, _
                       ByRef freeCells(,) As Boolean, Optional ByVal seed As Integer = -1)
            ActiveObjects = ao
            LocCenter = lc
            PreferedCapitalPosition = pcp
            placingObjects = po
            Term = t
            rndgen = New RndValueGen(seed)

            Dim freeSizeX As Integer = UBound(freeCells, 1)
            Dim freeSizeY As Integer = UBound(freeCells, 2)

            ReDim bestOutput(UBound(placingObjects)), output(UBound(placingObjects)), _
                  distance(UBound(freeCells, 1), UBound(freeCells, 2)), mayPlaceIfClean(UBound(placingObjects)), _
                  weightLayer(1)
            ReDim joinedOutput(UBound(output))
            For i As Integer = 0 To UBound(placingObjects) Step 1
                output(i) = -1
            Next i
            Dim p As New Point(0, 0)
            For y As Integer = 0 To freeSizeY Step 1
                For x As Integer = 0 To freeSizeX Step 1
                    distance(x, y) = p.Dist(x, y)
                Next x
            Next y
            For j As Integer = 0 To 1 Step 1
                ReDim weightLayer(j)(UBound(placingObjects))
            Next
            For i As Integer = 0 To UBound(placingObjects) Step 1
                ReDim mayPlaceIfClean(i)(freeSizeX, freeSizeY)
                For j As Integer = 0 To 1 Step 1
                    ReDim weightLayer(j)(i)(freeSizeX, freeSizeY)
                Next
                For y As Integer = 0 To freeSizeY Step 1
                    For x As Integer = 0 To freeSizeX Step 1
                        mayPlaceIfClean(i)(x, y) = MayPlaceObject(freeCells, placingObjects(i).objectType, x, y, ActiveObjects)
                    Next x
                Next y
            Next i

            ReDim free_initial_points(freeCells.Length - 1), free_initial_point_id(freeSizeX, freeSizeY)
            Dim freePointsCount As Integer = -1
            For y As Integer = 0 To freeSizeY Step 1
                For x As Integer = 0 To freeSizeX Step 1
                    If freeCells(x, y) Then
                        freePointsCount += 1
                        free_initial_points(freePointsCount) = New Point(x, y)
                        free_initial_point_id(x, y) = freePointsCount
                        For i As Integer = 0 To 1 Step 1
                            weightLayer(i)(0)(x, y) = 1
                        Next i
                    Else
                        free_initial_point_id(x, y) = -1
                    End If
                Next x
            Next y
            ReDim Preserve free_initial_points(freePointsCount)

            ReDim currentWeight(UBound(placingObjects))
            For i As Integer = 0 To UBound(currentWeight) Step 1
                ReDim currentWeight(i)(UBound(free_initial_points))
                If i = 0 Then
                    For j = 0 To UBound(free_initial_points) Step 1
                        currentWeight(i)(j) = 1
                    Next j
                End If
            Next i
        End Sub

        Public Shared Sub speedBanchmark()

            Dim locSize As Integer = 29

            Dim actObj() As AttendedObject = (New ImpenetrableMeshGen(New Common(GenDefaultValues.DefaultMod))).ActiveObjects
            Dim center As New Point(CInt(locSize / 2), CInt(locSize / 2))
            Dim free(locSize, locSize) As Boolean
            For y As Integer = 0 To locSize Step 1
                For x As Integer = 0 To locSize Step 1
                    If center.Dist(x, y) <= locSize / 2 Then free(x, y) = True
                Next x
            Next y

            Dim po() As ObjectPlacingSettings = New ObjectPlacingSettings() { _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Capital, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mine, .placeNearWith = 0, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mine, .placeNearWith = 0, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mine, .placeNearWith = 0, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.City, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mercenary, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Vendor, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Ruins, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Ruins, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mage, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mine, .placeNearWith = 4, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mine, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Mine, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Ruins, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}, _
                New ObjectPlacingSettings With {.objectType = DefMapObjects.Types.Trainer, .placeNearWith = -1, .applyUniformity = False, .prefferedDistance = 4, .sigma = 0.2}}

            Dim aop As New ActiveObjectsPlacer(actObj, center, center, po, New TerminationCondition(10000), free, 1234567)

            Dim t0 As Integer = Environment.TickCount
            Call aop.PlaceObjRow(0, free)
            Console.WriteLine("' " & aop.debug_checked & vbTab & aop.debug_discarded)
            Console.WriteLine("' " & Environment.TickCount - t0)
            ' 34010	5792
            ' 10000
        End Sub

        Public Structure ObjectPlacingSettings
            ''' <summary>Тип объекта</summary>
            Dim objectType As DefMapObjects.Types
            ''' <summary>Рядом с каким объектом разместить этот объект</summary>
            Dim placeNearWith As Integer
            ''' <summary>Растояние от объекта placeNearWith, на котором скорее всего окажется размещаемый объект</summary>
            Dim prefferedDistance As Double
            ''' <summary>Чем меньше сигма, тем строже требование по расстоянию</summary>
            Dim sigma As Double
            ''' <summary>Даже при заданном расстоянии от объекта стараться расположить объект с учетом других</summary>
            Dim applyUniformity As Boolean

            Public Shared Function Copy(ByRef v As ObjectPlacingSettings) As ObjectPlacingSettings
                Return New ObjectPlacingSettings With {.applyUniformity = v.applyUniformity, _
                                                        .objectType = v.objectType, _
                                                        .placeNearWith = v.placeNearWith, _
                                                        .prefferedDistance = v.prefferedDistance, _
                                                        .sigma = v.sigma}
            End Function

            Public Sub SetDistanceSettings(ByRef nearObjectType As DefMapObjects.Types, _
                                           ByRef ActiveObjects() As AttendedObject, ByRef rndgen As RndValueGen)
                If objectType = DefMapObjects.Types.Capital Then
                    prefferedDistance = 2
                    sigma = 2
                Else
                    prefferedDistance = 0.5 * rndgen.Rand(1.05, Math.Sqrt(2.1), True) * (ActiveObjects(objectType).Size + ActiveObjects(nearObjectType).Size) + rndgen.Rand(0, 3, True)
                    sigma = 0.2
                End If
            End Sub
        End Structure

        Friend Sub PlaceObjRow(ByRef n As Integer, _
                               ByRef freeCells(,) As Boolean)
            If Term.ExitFromLoops Then Exit Sub

            If n > 0 Then
                Dim hkey As String = PlacedObjectsKey(n - 1, output)
                If checkedVariants.Contains(hkey) Then
                    output(n) = -1
                    debug_discarded += 1
                    Exit Sub
                Else
                    debug_checked += 1
                    checkedVariants.Add(hkey)
                End If
            End If

            If n > maxN Then
                For i As Integer = 0 To n - 1 Step 1
                    bestOutput(i) = New Point(free_initial_points(output(i)).X, free_initial_points(output(i)).Y)
                Next i
                maxN = n
            End If

            Dim selected As Integer

            'считаем weightlayer(n)
            Call CalcLayerWeight(n - 1)

            Dim pID As List(Of Integer) = Nothing

            Call CalcWeight(freeCells, n, currentWeight(n), pID)

            If pID.Count = 0 Then
                output(n) = -1
                Exit Sub
            End If

            Dim checkN As Integer = Math.Min(10, pID.Count)
            If n < UBound(placingObjects) Then
                Do While pID.Count > 0
                    selected = rndgen.RandomSelection(pID, currentWeight(n), False, True)
                    pID.Remove(selected)
                    output(n) = CShort(selected)
                    Call ChangeObjectState(freeCells, placingObjects(n).objectType, _
                                           free_initial_points(output(n)), _
                                           ActiveObjects, False)
                    Call PlaceObjRow(n + 1, freeCells)
                    Call ChangeObjectState(freeCells, placingObjects(n).objectType, _
                                           free_initial_points(output(n)), _
                                           ActiveObjects, True)
                    If Term.ExitFromLoops Then Exit Sub
                    If output(n + 1) > -1 Then
                        pID.Clear()
                    Else
                        checkN -= 1
                        If checkN = 0 Then
                            Call Term.CheckTime()
                            If Term.ExitFromLoops Then Exit Sub
                            checkN = 10
                        End If
                    End If
                Loop
                If output(n + 1) = -1 Then output(n) = -1
            Else
                selected = rndgen.RandomSelection(pID, currentWeight(n), False, True)
                pID.Clear()
                output(n) = CShort(selected)
            End If
        End Sub

        Private Sub CalcLayerWeight(ByRef addedN As Integer)
            If addedN > 0 Then
                Dim oX As Integer = ShiftCoordinate(free_initial_points(output(addedN)).X, addedN)
                Dim oY As Integer = ShiftCoordinate(free_initial_points(output(addedN)).Y, addedN)

                Dim R As Double
                If addedN > 1 Then
                    Dim prevN As Integer = addedN - 1
                    For Each freeP As Point In free_initial_points
                        Dim x As Integer = freeP.X
                        Dim y As Integer = freeP.Y
                        Dim sX As Integer = ShiftCoordinate(x, addedN + 1)
                        Dim sY As Integer = ShiftCoordinate(y, addedN + 1)

                        R = GetDist(oX, oY, sX, sY)
                        For i As Integer = 0 To 1 Step 1
                            weightLayer(i)(addedN)(x, y) = weightLayer(i)(prevN)(x, y) * (1 + rWeightMultiplier(i) * R)
                        Next i
                    Next freeP
                Else
                    For Each freeP As Point In free_initial_points
                        Dim x As Integer = freeP.X
                        Dim y As Integer = freeP.Y
                        Dim sX As Integer = ShiftCoordinate(x, addedN + 1)
                        Dim sY As Integer = ShiftCoordinate(y, addedN + 1)

                        R = GetDist(oX, oY, sX, sY)
                        For i As Integer = 0 To 1 Step 1
                            weightLayer(i)(addedN)(x, y) = 1 + rWeightMultiplier(i) * R
                        Next i
                    Next freeP
                End If
            End If
        End Sub
        Private Sub CalcWeight(ByRef fc_bak(,) As Boolean, ByRef n As Integer, _
                               ByRef Weight() As Double, _
                               ByRef pID As List(Of Integer))
            pID = New List(Of Integer)

            Dim R As Double
            If placingObjects(n).placeNearWith > -1 Or placingObjects(n).objectType = DefMapObjects.Types.Capital Then
                For Each freeP As Point In free_initial_points
                    Dim x As Integer = freeP.X
                    Dim y As Integer = freeP.Y
                    Dim sX As Integer = ShiftCoordinate(x, n)
                    Dim sY As Integer = ShiftCoordinate(y, n)

                    If mayPlaceIfClean(n)(x, y) AndAlso MayPlaceObject(fc_bak, placingObjects(n).objectType, x, y, ActiveObjects) Then
                        Dim id As Integer = free_initial_point_id(x, y)
                        pID.Add(id)
                        If placingObjects(n).placeNearWith > -1 Then
                            Dim oX As Integer = ShiftCoordinate(free_initial_points(output(placingObjects(n).placeNearWith)).X, placingObjects(n).placeNearWith)
                            Dim oY As Integer = ShiftCoordinate(free_initial_points(output(placingObjects(n).placeNearWith)).Y, placingObjects(n).placeNearWith)
                            R = GetDist(oX, oY, sX, sY)
                        Else
                            If Not placingObjects(n).objectType = DefMapObjects.Types.Capital Then
                                R = GetDist(LocCenter, sX, sY)
                            Else
                                R = GetDist(PreferedCapitalPosition, sX, sY)
                            End If
                        End If
                        Weight(id) = Common.Gauss(R, placingObjects(n).prefferedDistance, placingObjects(n).sigma)
                        If placingObjects(n).applyUniformity Then
                            Weight(id) *= weightLayer(0)(n - 1)(x, y)
                        End If
                        Weight(id) = Math.Max(Weight(id), 0.000001)
                    End If
                Next freeP
            Else
                For Each freeP As Point In free_initial_points
                    Dim x As Integer = freeP.X
                    Dim y As Integer = freeP.Y
                    Dim sX As Integer = ShiftCoordinate(x, n)
                    Dim sY As Integer = ShiftCoordinate(y, n)

                    If mayPlaceIfClean(n)(x, y) AndAlso MayPlaceObject(fc_bak, placingObjects(n).objectType, x, y, ActiveObjects) Then
                        Dim id As Integer = free_initial_point_id(x, y)
                        pID.Add(id)
                        If placingObjects(n).objectType = DefMapObjects.Types.Mine Then
                            For i As Integer = 0 To n - 1 Step 1
                                If placingObjects(i).objectType = DefMapObjects.Types.Mine Then
                                    Dim oX As Integer = ShiftCoordinate(free_initial_points(output(i)).X, i)
                                    Dim oY As Integer = ShiftCoordinate(free_initial_points(output(i)).Y, i)

                                    R = GetDist(oX, oY, sX, sY)
                                    Weight(id) *= (1 + 0.2 * R * R)
                                End If
                            Next i
                        End If
                        If n > 0 Then Weight(id) = Math.Max(weightLayer(1)(n - 1)(x, y), 0.000001)
                    End If
                Next freeP
            End If
        End Sub

        Private Function ShiftCoordinate(ByRef v As Integer, ByRef objectN As Integer) As Integer
            Return v + ActiveObjects(placingObjects(objectN).objectType).SizeHalf
        End Function

        Private Function GetDist(ByRef p1 As Point, ByRef x2 As Integer, ByRef y2 As Integer) As Double
            Return GetDist(p1.X, p1.Y, x2, y2)
        End Function
        Private Function GetDist(ByRef x1 As Integer, ByRef y1 As Integer, ByRef x2 As Integer, ByRef y2 As Integer) As Double
            Return distance(Math.Abs(x1 - x2), Math.Abs(y1 - y2))
        End Function

        Friend Shared Function ObjectBorders(ByRef id As DefMapObjects.Types, ByRef x As Integer, ByRef y As Integer, _
                                             ByRef ActiveObjects() As AttendedObject) As Location.Borders
            Dim res As Location.Borders
            res.minX = x - 1
            res.minY = y - 1
            res.maxX = x + ActiveObjects(id).Size
            res.maxY = y + ActiveObjects(id).Size
            If ActiveObjects(id).hasExternalGuard Then
                res.minX -= 1
                res.minY -= 1
                res.maxX += 1
                res.maxY += 1
            End If
            Return res
        End Function
        Friend Shared Function MayPlaceObject(ByRef m As Map, ByRef id As DefMapObjects.Types, _
                                              ByRef x As Integer, ByRef y As Integer, _
                                              ByRef ActiveObjects() As AttendedObject) As Boolean
            If m.board(x, y).passability.isBorder Or m.board(x, y).passability.isAttended Then Return False
            Dim b As Location.Borders = ObjectBorders(id, x, y, ActiveObjects)
            If b.minX < 0 Or b.minY < 0 Or b.maxX > m.xSize Or b.maxY > m.ySize Then Return False
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    If m.board(i, j).passability.isBorder Or m.board(i, j).passability.isAttended Or Not m.board(i, j).locID(0) = id Then Return False
                Next i
            Next j
            Return True
        End Function
        Friend Shared Function MayPlaceObject(ByRef freeCell(,) As Boolean, ByRef id As DefMapObjects.Types, _
                                              ByRef x As Integer, ByRef y As Integer, _
                                              ByRef ActiveObjects() As AttendedObject) As Boolean
            If Not freeCell(x, y) Then Return False
            Dim b As Location.Borders = ObjectBorders(id, x, y, ActiveObjects)
            If b.minX < 0 Or b.minY < 0 Or b.maxX > UBound(freeCell, 1) Or b.maxY > UBound(freeCell, 2) Then Return False
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    If Not freeCell(i, j) Then Return False
                Next i
            Next j
            Return True
        End Function
        Friend Shared Sub PlaceObject(ByRef m As Map, ByRef id As DefMapObjects.Types, _
                                      ByRef x As Integer, ByRef y As Integer, _
                                      ByRef GroupID As Integer, ByRef placedCities As Integer, _
                                      ByRef settLoc As Map.SettingsLoc, ByRef settMap As Map.SettingsMap, _
                                      ByRef ActiveObjects() As AttendedObject, ByRef symm As SymmetryOperations)
            If m.symmID < 0 Then
                Dim b As Location.Borders = ObjectBorders(id, x, y, ActiveObjects)
                For j As Integer = b.minY To b.maxY Step 1
                    For i As Integer = b.minX To b.maxX Step 1
                        m.board(i, j).passability.isAttended = True
                    Next i
                Next j
                Call PlaceObject_SetProperties(m, id, GroupID, placedCities, settLoc, settMap, 0, b.minX, b.minY)
                placedCities += 1
            Else
                Dim b As Location.Borders = ObjectBorders(id, x, y, ActiveObjects)
                Dim p(3), plist() As Point
                For k As Integer = 0 To UBound(p) Step 1
                    p(k) = New Point(Integer.MaxValue, Integer.MaxValue)
                Next k
                For j As Integer = b.minY To b.maxY Step 1
                    For i As Integer = b.minX To b.maxX Step 1
                        plist = symm.ApplySymm(New Point(i, j), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(plist) Step 1
                            m.board(plist(k).X, plist(k).Y).passability.isAttended = True
                            If p(k).X >= plist(k).X And p(k).Y >= plist(k).Y Then p(k) = New Point(plist(k).X, plist(k).Y)
                        Next k
                    Next i
                Next j
                Dim ownerIncrement As Integer = 0
                For k As Integer = 0 To UBound(p) Step 1
                    If p(k).X < Integer.MaxValue And p(k).Y < Integer.MaxValue Then
                        Call PlaceObject_SetProperties(m, id, GroupID, placedCities, settLoc, settMap, ownerIncrement, p(k).X, p(k).Y)
                        ownerIncrement += 1
                    End If
                Next k
                placedCities += 1
            End If
        End Sub
        Private Shared Sub PlaceObject_SetProperties(ByRef m As Map, ByRef typeID As DefMapObjects.Types, _
                                                     ByRef GroupID As Integer, ByRef placedCities As Integer, _
                                                     ByRef settLoc As Map.SettingsLoc, ByRef settMap As Map.SettingsMap, _
                                                     ByRef ownerIncrement As Integer, _
                                                     ByRef x As Integer, ByRef y As Integer)
            m.board(x, y).mapObject.objectID = typeID
            m.board(x, y).groupID = GroupID
            If typeID = DefMapObjects.Types.City Then
                If Not IsNothing(settLoc.RaceCities) AndAlso placedCities < settLoc.RaceCities.Length Then
                    m.board(x, y).mapObject.City = Map.SettingsLoc.SettingsRaceCity.Copy(settLoc.RaceCities(placedCities))
                    Call m.board(x, y).mapObject.City.IncrementOwner(ownerIncrement, settMap.nRaces)
                End If
            End If
        End Sub
        Friend Shared Sub ChangeObjectState(ByRef freeCell(,) As Boolean, ByRef id As DefMapObjects.Types, _
                                            ByRef pos As Point, _
                                            ByRef ActiveObjects() As AttendedObject, _
                                            ByRef removeIt As Boolean)
            Dim b As Location.Borders = ObjectBorders(id, pos.X, pos.Y, ActiveObjects)
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    freeCell(i, j) = removeIt
                Next i
            Next j
        End Sub
        Private Function PlacedObjectsKey(ByRef n As Integer, ByRef output() As Short) As String
            joinedOutput(n) = ValueConverter.BitJoin(ActiveObjects(placingObjects(n).objectType).sArea, output(n))
            Dim res(n) As Integer
            For i As Integer = 0 To n Step 1
                res(i) = joinedOutput(i)
            Next i
            'Console.WriteLine(String.Join(".", res))
            Call Array.Sort(res)
            Dim result As String = ""
            For i As Integer = 0 To n Step 1
                result &= ValueConverter.ToChrString(res(i))
            Next i
            Return result
        End Function

        Public Shared Function DefineCapitalPreferedPos(ByVal settMap As Map.SettingsMap, ByVal LocFreeCells()(,) As Boolean, _
                                                        ByVal ActiveObjects() As AttendedObject) As Point()
            Dim result(UBound(LocFreeCells)) As Point
            Dim freePoints(settMap.nRaces - 1)() As Point
            Dim possiblePoints(settMap.nRaces - 1) As List(Of Point)
            Dim dist(settMap.nRaces - 1)() As Double
            Dim d(settMap.nRaces - 1) As GenSettings.LocationGenSetting.ValueRange
            Dim m As New GenSettings.LocationGenSetting.ValueRange

            Dim mapCenter As New Point(CInt(settMap.xSize / 2), CInt(settMap.ySize / 2))
            Parallel.For(0, settMap.nRaces,
             Sub(i As Integer)
                 ReDim freePoints(i)(-1), dist(i)(-1)
                 d(i) = New GenSettings.LocationGenSetting.ValueRange
                 d(i).min = Double.MaxValue
                 d(i).max = Double.MinValue
                 Dim freeSizeX As Integer = UBound(LocFreeCells(i), 1)
                 Dim freeSizeY As Integer = UBound(LocFreeCells(i), 2)
                 For y As Integer = 0 To freeSizeY Step 1
                     For x As Integer = 0 To freeSizeX Step 1
                         If MayPlaceObject(LocFreeCells(i), DefMapObjects.Types.Capital, x, y, ActiveObjects) Then
                             ReDim Preserve freePoints(i)(freePoints(i).Length), dist(i)(dist(i).Length)
                             freePoints(i)(UBound(freePoints(i))) = New Point(x, y)
                             dist(i)(UBound(dist(i))) = mapCenter.Dist(x, y)
                             d(i).min = Math.Min(d(i).min, dist(i)(UBound(dist(i))))
                             d(i).max = Math.Max(d(i).max, dist(i)(UBound(dist(i))))
                         End If
                     Next x
                 Next y
             End Sub)
            m.min = d(0).min
            m.max = d(0).max
            For i As Integer = 1 To settMap.nRaces - 1 Step 1
                m.min = Math.Max(m.min, d(i).min)
                m.max = Math.Min(m.max, d(i).max)
            Next i
            If m.min > m.max Then
                m.min = 0.5 * (m.min + m.max)
                m.max = m.min
            End If
            Dim distDispersion As Double = 1.1
            For i As Integer = 0 To settMap.nRaces - 1 Step 1
                If d(i).min < m.min Then
                    If d(i).max > m.min Then
                        d(i).min = m.min
                    Else
                        d(i).min = d(i).max
                    End If
                End If
                If d(i).max > m.max Then
                    If d(i).min < m.max Then
                        d(i).max = m.max
                    Else
                        d(i).max = d(i).min
                    End If
                End If
            Next i

            Parallel.For(0, settMap.nRaces,
             Sub(i As Integer)
                 possiblePoints(i) = New List(Of Point)
                 Dim R As Double = m.min + settMap.CapitalRepulsion * (m.max - m.min)
                 If R > d(i).max Then
                     R = d(i).max
                 ElseIf R < d(i).min Then
                     R = d(i).min
                 End If
                 For j As Integer = 0 To UBound(freePoints(i)) Step 1
                     If Math.Abs(dist(i)(j) - R) <= distDispersion Then
                         possiblePoints(i).Add(freePoints(i)(j))
                     End If
                 Next j
             End Sub)

            Dim attempts As Integer
            For i As Integer = 0 To settMap.nRaces - 1 Step 1
                attempts += possiblePoints(i).Count
            Next i
            attempts *= 4
            Dim rndgen As New RndValueGen
            Dim t(settMap.nRaces - 1) As Point
            Dim distSum, maxSum As Double
            For p As Integer = 0 To attempts Step 1
                For i As Integer = 0 To settMap.nRaces - 1 Step 1
                    Dim n As Integer = rndgen.RndIntFast(0, possiblePoints(i).Count - 1)
                    t(i) = possiblePoints(i).Item(n)
                Next i
                distSum = 0
                For i2 As Integer = 0 To settMap.nRaces - 2 Step 1
                    For i1 As Integer = i2 To settMap.nRaces - 1 Step 1
                        distSum += t(i1).SqDist(t(i2))
                    Next i1
                Next i2
                If maxSum < distSum OrElse (maxSum = distSum AndAlso rndgen.RndIntFast(0, 1) = 1) Then
                    maxSum = distSum
                    For i As Integer = 0 To settMap.nRaces - 1 Step 1
                        result(i) = t(i)
                    Next i
                End If
            Next p
            Return result
        End Function
    End Class

    ''' <summary>Выполняется после SetBorders. Расставит посещаемые объекты</summary>
    ''' <param name="m">Хранилище данных о карте. К этому моменту должны быть 
    ''' присвоены значения переменным .xSize и .ySize (например, для карты 96x48 значения 95 и 47, соответственно)
    ''' присвоено значение для symmID - ID примененной операции симметрии (см. класс SymmetryOperations) (-1 - без симметрии)
    ''' инициализированы массивы.
    ''' .Loc(from 0 to "количество локаций-1") и .board(from 0 to xSize, from 0 to ySize).
    ''' .Loc() - внутри инициализированные локации.
    ''' .board(,).LocID - как минимум одно значение (можно больше, если по соседству с тайлом есть тайлы других локаций (но тот, что первый в списке - основной).
    ''' Непроходимые тайлы на границе локаций (внутри локаций следует их добавлять уже после расстановки посещаемых объектов) - .board(,).passability.isBorder = True;
    ''' Для тайлов, являющихся проходом между локациями - .board(,).passability.isPass = True;
    ''' остальные записи в board(,) могут быть пустыми, неинициализированными и всё такое.</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт</param>
    ''' <param name="Term">Нужно инициализировать экземпляр этого класса до начала генерации</param>
    Public Sub PlaceActiveObjects(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                                  ByRef settRaceLoc As Map.SettingsLoc, ByVal settCommLoc As Map.SettingsLoc, _
                                  ByRef Term As TerminationCondition)
        Dim a() As Map.SettingsLoc = Map.SettingsLoc.ToArray(settRaceLoc, settCommLoc, settMap.nRaces, m.Loc.Length)
        Call PlaceActiveObjects(m, settMap, a, Term)
    End Sub
    ''' <summary>Расставит посещаемые объекты</summary>
    ''' <param name="m">Хранилище данных о карте. К этому моменту должны быть 
    ''' присвоены значения переменным .xSize и .ySize (например, для карты 96x48 значения 95 и 47, соответственно)
    ''' присвоено значение для symmID - ID примененной операции симметрии (см. класс SymmetryOperations) (-1 - без симметрии)
    ''' инициализированы массивы.
    ''' .Loc(from 0 to "количество локаций-1") и .board(from 0 to xSize, from 0 to ySize).
    ''' .Loc() - внутри инициализированные локации (первыми должны идти стартовые локации рас).
    ''' .board(,).LocID - как минимум одно значение (можно больше, если по соседству с тайлом есть тайлы других локаций (но тот, что первый в списке - основной).
    ''' Непроходимые тайлы на границе локаций (внутри локаций следует их добавлять уже после расстановки посещаемых объектов) - .board(,).passability.isBorder = True;
    ''' Для тайлов, являющихся проходом между локациями - .board(,).passability.isPass = True;
    ''' остальные записи в board(,) могут быть пустыми, неинициализированными и всё такое.</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settLoc">Настройки для каждой локации. Первыми должны идти стартовые локации рас.
    ''' Коментарий к настройкам стартовых локаций играбельных рас:
    ''' дробная часть определяет шанс округления большую сторону.
    ''' Комментарий к настройкам остальных локаций:
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади, заданной в настройках (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт </param>
    ''' <param name="Term">Нужно инициализировать экземпляр этого класса до начала генерации</param>
    Public Sub PlaceActiveObjects(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                                  ByVal settLoc() As Map.SettingsLoc, _
                                  ByRef Term As TerminationCondition)
        ObjectBlank = ActiveObjectsSet(settMap, m.symmID)
        Dim tmpm As Map = m
        Dim LocsPlacing(UBound(tmpm.Loc)) As Location.Borders
        Dim LocArea(UBound(tmpm.Loc))() As Integer
        Dim LocSymmMult(UBound(tmpm.Loc)) As Double
        Dim LocFreeCells(UBound(tmpm.Loc))(,) As Boolean

        Parallel.For(0, tmpm.Loc.Length, _
         Sub(Li As Integer)
             Dim id As Integer = tmpm.Loc(Li).ID
             LocsPlacing(Li) = New Location.Borders With {.minX = Integer.MaxValue, .minY = Integer.MaxValue, _
                                                          .maxX = Integer.MinValue, .maxY = Integer.MinValue}
             LocArea(id - 1) = New Integer() {0, 0}
             For y As Integer = 0 To tmpm.ySize Step 1
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If tmpm.board(x, y).locID(0) = id Then
                         LocsPlacing(Li).minX = Math.Min(LocsPlacing(Li).minX, x)
                         LocsPlacing(Li).minY = Math.Min(LocsPlacing(Li).minY, y)
                         LocsPlacing(Li).maxX = Math.Max(LocsPlacing(Li).maxX, x)
                         LocsPlacing(Li).maxY = Math.Max(LocsPlacing(Li).maxY, y)
                     End If
                 Next x
             Next y
             Dim dx As Integer = LocsPlacing(id - 1).minX
             Dim dy As Integer = LocsPlacing(id - 1).minY
             Dim freeCells(LocsPlacing(id - 1).maxX - dx, _
                           LocsPlacing(id - 1).maxY - dy) As Boolean
             For y As Integer = LocsPlacing(id - 1).minY To LocsPlacing(id - 1).maxY Step 1
                 For x As Integer = LocsPlacing(id - 1).minX To LocsPlacing(id - 1).maxX Step 1
                     If Not tmpm.board(x, y).passability.isBorder _
                      And Not tmpm.board(x, y).passability.isAttended _
                      AndAlso tmpm.board(x, y).locID(0) = id Then
                         LocArea(id - 1)(0) += 1
                         freeCells(x - dx, y - dy) = True
                     End If
                 Next x
             Next y

             Dim fc(,) As Boolean = CType(freeCells.Clone, Boolean(,))
             If tmpm.symmID > -1 Then
                 Dim p() As Point
                 For y As Integer = 0 To UBound(freeCells, 2) Step 1
                     For x As Integer = 0 To UBound(freeCells, 1) Step 1
                         If freeCells(x, y) Then
                             p = symm.ApplySymm(New Point(x + dx, y + dy), settMap.nRaces, tmpm, 1)
                             For i As Integer = 0 To UBound(p) Step 1
                                 Dim tx As Integer = p(i).X - dx
                                 Dim ty As Integer = p(i).Y - dy
                                 If Not tx = x Or Not ty = y Then
                                     If tx > -1 And ty > -1 And tx <= UBound(freeCells, 1) And ty <= UBound(freeCells, 2) Then
                                         Dim b As Location.Borders = NearestXY(tx, ty, UBound(freeCells, 1), UBound(freeCells, 2), 1)
                                         For q As Integer = b.minY To b.maxY Step 1
                                             For w As Integer = b.minX To b.maxX Step 1
                                                 freeCells(w, q) = False
                                             Next w
                                         Next q
                                     End If
                                 End If
                             Next i
                         End If
                     Next x
                 Next y
             End If
             Dim PlaceCells(UBound(freeCells, 1), UBound(freeCells, 2)) As Boolean
             For y As Integer = 0 To UBound(freeCells, 2) Step 1
                 For x As Integer = 0 To UBound(freeCells, 1) Step 1
                     If freeCells(x, y) AndAlso ActiveObjectsPlacer.MayPlaceObject(freeCells, DefMapObjects.Types.Mine, x, y, ActiveObjects) Then
                         Dim b As Location.Borders = NearestXY(x, y, UBound(freeCells, 1), UBound(freeCells, 2), 1)
                         For q As Integer = b.minY To b.maxY Step 1
                             For p As Integer = b.minX To b.maxX Step 1
                                 PlaceCells(p, q) = True
                             Next p
                         Next q
                     End If
                 Next x
             Next y

             For y As Integer = 0 To UBound(freeCells, 2) Step 1
                 For x As Integer = 0 To UBound(freeCells, 1) Step 1
                     If PlaceCells(x, y) Then LocArea(id - 1)(1) += 1
                 Next x
             Next y
             If LocArea(id - 1)(0) > 0 And LocArea(id - 1)(1) = 0 Then
                 Dim ids As New List(Of Integer)
                 Dim p(freeCells.Length - 1) As Point
                 Dim n As Integer = -1
                 For y As Integer = 1 To UBound(freeCells, 2) - 1 Step 1
                     For x As Integer = 1 To UBound(freeCells, 1) - 1 Step 1
                         If freeCells(x, y) Then
                             n += 1
                             p(n) = New Point(x, y)
                             ids.Add(n)
                         End If
                     Next x
                 Next y
                 If ids.Count > 0 Then
                     Dim s As Integer = comm.RandomSelection(ids, True)
                     Dim x As Integer = p(s).X
                     Dim y As Integer = p(s).Y
                     For qq As Integer = y - 1 To y + 1 Step 1
                         For pp As Integer = x - 1 To x + 1 Step 1
                             freeCells(pp, qq) = True
                             LocArea(id - 1)(1) += 1
                         Next pp
                     Next qq
                 End If
             End If
             LocFreeCells(id - 1) = freeCells

             If tmpm.symmID > -1 Then
                 Dim n As Integer = 0
                 Dim pp() As Point = New Point() {New Point(LocsPlacing(Li).minX, LocsPlacing(Li).minY), _
                                                  New Point(LocsPlacing(Li).minX, LocsPlacing(Li).maxY), _
                                                  New Point(LocsPlacing(Li).maxX, LocsPlacing(Li).minY), _
                                                  New Point(LocsPlacing(Li).maxX, LocsPlacing(Li).maxY)}
                 Dim p() As Point = symm.ApplySymm(pp(0), settMap.nRaces, tmpm, 1)
                 For q As Integer = 0 To UBound(p) Step 1
                     For w As Integer = 0 To UBound(pp) Step 1
                         If p(q).X = pp(w).X And p(q).Y = pp(w).Y Then
                             n += 1
                             Exit For
                         End If
                     Next w
                 Next q
                 LocSymmMult(Li) = 1 / n
             Else
                 LocSymmMult(Li) = 1
             End If
         End Sub)

        Dim GroupID As Integer = 0
        Dim maxTime As Long = Term.maxTime
        Dim R0 As ObjectPlacingResult = FillLocation(1, tmpm, LocsPlacing, LocArea, settMap, settLoc, _
                                                     tmpm.symmID, True, LocSymmMult, LocFreeCells, maxTime)
        If Not R0.msg = "" Then Throw New Exception(R0.msg)

        Dim R1(UBound(tmpm.Loc)) As ObjectPlacingResult
        Parallel.For(settMap.nRaces, tmpm.Loc.Length, _
         Sub(i As Integer)
             'For i As Integer = settMap.nRaces To UBound(tmpm.Loc) Step 1
             If Not tmpm.Loc(i).IsObtainedBySymmery Then
                 R1(i) = FillLocation(tmpm.Loc(i).ID, tmpm, LocsPlacing, LocArea, settMap, settLoc, _
                                      tmpm.symmID, False, LocSymmMult, LocFreeCells, maxTime)
             End If
             'Next i
         End Sub)
        For i As Integer = settMap.nRaces To UBound(tmpm.Loc) Step 1
            If Not R1(i).msg = "" Then Throw New Exception(R1(i).msg)
        Next i

        Call PlaceSelectedObjects(tmpm, settMap, settLoc, R0, GroupID)
        For i As Integer = settMap.nRaces To UBound(tmpm.Loc) Step 1
            Call PlaceSelectedObjects(tmpm, settMap, settLoc, R1(i), GroupID)
        Next i

        If Term.ExitFromLoops Then Exit Sub

        Dim maxGroupID As Integer = ImpenetrableMeshGen.GetMaxGroupID(tmpm)

        For y As Integer = tmpm.ySize To 0 Step -1
            For x As Integer = tmpm.xSize To 0 Step -1
                If tmpm.board(x, y).mapObject.objectID > DefMapObjects.Types.None Then
                    Dim id As Integer = tmpm.board(x, y).mapObject.objectID
                    Dim gid As Integer = tmpm.board(x, y).groupID
                    Dim t_city As Map.SettingsLoc.SettingsRaceCity = Map.SettingsLoc.SettingsRaceCity.Copy(tmpm.board(x, y).mapObject.City)
                    Dim d As Integer = ActiveObjects(id).dxy
                    For j As Integer = 0 To UBound(ObjectBlank(id), 2) Step 1
                        Dim dy As Integer = y + j
                        For i As Integer = 0 To UBound(ObjectBlank(id), 1) Step 1
                            Dim dx As Integer = x + i
                            If dx <= tmpm.xSize And dy <= tmpm.ySize Then
                                tmpm.board(dx, dy).groupID = ObjectBlank(id)(i, j).groupID
                                tmpm.board(dx, dy).stack.GuardLoc = ObjectBlank(id)(i, j).stack.GuardLoc
                                If tmpm.board(dx, dy).stack.GuardLoc Then tmpm.board(dx, dy).stack.ObjectGuard = True
                                tmpm.board(dx, dy).passability.isAttended = ObjectBlank(id)(i, j).passability.isAttended
                                tmpm.board(dx, dy).passability.isBorder = ObjectBlank(id)(i, j).passability.isBorder
                                tmpm.board(dx, dy).passability.isPenetrable = ObjectBlank(id)(i, j).passability.isPenetrable
                                tmpm.board(dx, dy).mapObject.objectID = ObjectBlank(id)(i, j).mapObject.objectID
                                tmpm.board(dx, dy).mapObject.City = Map.SettingsLoc.SettingsRaceCity.Copy(ObjectBlank(id)(i, j).mapObject.City)
                                If ObjectBlank(id)(i, j).passability.isAttended Or ObjectBlank(id)(i, j).passability.isBorder Then
                                    tmpm.board(dx, dy).passability.isPass = False
                                End If
                                If tmpm.board(dx, dy).stack.GuardLoc And tmpm.board(dx, dy).mapObject.objectID = DefMapObjects.Types.City _
                                 And tmpm.board(dx, dy).mapObject.City.owner > 0 Then
                                    tmpm.board(dx, dy).stack.GuardLoc = False
                                End If
                            ElseIf ObjectBlank(id)(i, j).stack.GuardLoc Or ObjectBlank(id)(i, j).passability.isAttended Or ObjectBlank(id)(i, j).passability.isBorder _
                            Or ObjectBlank(id)(i, j).mapObject.objectID > DefMapObjects.Types.None Then
                                Throw New Exception("Неправильно поставлен объект: objectID = " & id & " position = " & x & "; " & y)
                            End If
                        Next i
                    Next j
                    tmpm.board(x + d, y + d).groupID = gid
                    tmpm.board(x + d, y + d).mapObject.City = Map.SettingsLoc.SettingsRaceCity.Copy(t_city)
                    If ActiveObjects(id).hasExternalGuard Then
                        maxGroupID += 1
                        tmpm.board(x + d + ActiveObjects(id).Size, _
                                   y + d + ActiveObjects(id).Size).groupID = maxGroupID 'gid
                    End If
                End If
            Next x
        Next y
    End Sub
    Private Sub ObjectsPlacingVariants(ByRef placingObjects() As ActiveObjectsPlacer.ObjectPlacingSettings, _
                                       ByRef locID As Integer, _
                                       ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                       ByRef LocsPlacing() As Location.Borders, _
                                       ByRef FreeCells(,) As Boolean, _
                                       ByRef CapitalPrefferedPos As Point, _
                                       ByRef output() As Point, ByRef maxTime As Long)

        Dim Term As New TerminationCondition(maxTime)

        Dim locCenter As New Point(m.Loc(locID - 1).pos.X - LocsPlacing(locID - 1).minX, _
                                   m.Loc(locID - 1).pos.Y - LocsPlacing(locID - 1).minY)

        Dim objPlacer As New ActiveObjectsPlacer(ActiveObjects, locCenter, CapitalPrefferedPos, placingObjects, Term, FreeCells)
        Call objPlacer.PlaceObjRow(0, FreeCells)
        output = objPlacer.bestOutput
        If objPlacer.maxN = 0 And IsNothing(output(0)) Then
            output = Nothing
            Exit Sub
        ElseIf IsNothing(output(0)) Then
            ReDim Preserve output(objPlacer.maxN - 1)
        End If
        For i As Integer = 0 To UBound(output) Step 1
            If Not IsNothing(output(i)) Then
                output(i) = New Point(output(i).X + LocsPlacing(locID - 1).minX, _
                                      output(i).Y + LocsPlacing(locID - 1).minY)
            Else
                ReDim Preserve output(i - 1)
                Exit For
            End If
        Next i
    End Sub
    Private Sub MakeLocObjectsList(ByRef places() As ActiveObjectsPlacer.ObjectPlacingSettings, ByRef loc As Location, _
                                   ByRef sett As Map.SettingsLoc, ByRef isRaceLoc As Boolean, _
                                   ByRef LocArea() As Integer, ByRef LocSymmMult As Double, ByRef map As Map)
        Dim nCapital, nMinMines As Integer
        Dim mult As Double
        If sett.scaleContent Then
            mult = LocSymmMult * LocArea(0) / (Math.PI * (sett.AverageRadius - 2) ^ 2)
        Else
            mult = 1
        End If
        If isRaceLoc Then
            nCapital = 1
            nMinMines = 3
        Else
            nCapital = 0
            If mult > 0.6 Then
                nMinMines = 2
            Else
                nMinMines = 1
            End If
        End If

        Dim areaUsed As Integer
        Dim DblnObj() As Double = New Double() {0, _
                                                nCapital, _
                                                sett.maxCities, _
                                                sett.maxVendors, _
                                                sett.maxMercenaries, _
                                                sett.maxMages, _
                                                sett.maxTrainers, _
                                                sett.maxRuins, _
                                                sett.maxGoldMines, _
                                                sett.maxManaSources}
        'определяем количество объектов
        Dim nObj(UBound(DblnObj) - 1) As Integer
        For i As Integer = 0 To UBound(DblnObj) Step 1
            Dim m As Double = DblnObj(i)
            If i > 1 Then m *= mult
            Dim n1 As Integer = CInt(Math.Floor(m))
            Dim n2 As Integer = CInt(Math.Ceiling(m))
            Dim r As Integer
            If n1 = n2 Then
                r = n1
            Else
                Dim d As Double = m - CDbl(n1)
                If rndgen.PRand(0, 1) > d Then
                    r = n1
                    If Not i = 8 And Not i = 9 Then DblnObj(8) += d
                Else
                    r = n2
                End If
            End If
            If i = 8 Or i = 9 Then
                nObj(8) += r
            Else
                nObj(i) = r
            End If
        Next i
        nObj(8) = Math.Max(nObj(8), 1)

        Dim sum As Integer = -1
        For i As Integer = 0 To UBound(nObj) Step 1
            sum += nObj(i)
        Next i
        If Not IsNothing(sett.RaceCities) Then sum += sett.RaceCities.Length


        ReDim places(sum)
        Dim p As Integer = 0

        'добавляем столицу
        Do While nObj(1) > 0
            Call AddObjId(places, nObj, DefMapObjects.Types.Capital, p, areaUsed)
            If areaUsed >= LocArea(1) Then
                ReDim Preserve places(p - 1)
                Call SetNearWithSettings(places, loc, map)
                Exit Sub
            End If
        Loop

        Dim course() As Integer
        If isRaceLoc Then
            course = {0, 1}
        Else
            course = {1, 0}
        End If

        For addCourseID As Integer = 0 To UBound(course) Step 1
            If course(addCourseID) = 0 Then
                'добавляем минимально нужное количество шахт
                Dim k As Integer = 0
                Dim placedMines As Integer = 0
                Do While nObj(8) > 0 And placedMines < nMinMines
                    Call AddObjId(places, nObj, DefMapObjects.Types.Mine, p, areaUsed)
                    If areaUsed >= LocArea(1) Then
                        ReDim Preserve places(p - 1)
                        Call SetNearWithSettings(places, loc, map)
                        Exit Sub
                    End If
                    placedMines += 1
                Loop
            ElseIf course(addCourseID) = 1 Then
                'добавляем обязательные города
                If Not IsNothing(sett.RaceCities) Then
                    For i As Integer = 0 To UBound(sett.RaceCities) Step 1
                        Call AddObjId(places, Nothing, DefMapObjects.Types.City, p, areaUsed)
                        If areaUsed >= LocArea(1) Then
                            ReDim Preserve places(p - 1)
                            Call SetNearWithSettings(places, loc, map)
                            Exit Sub
                        End If
                    Next i
                End If
            End If
        Next addCourseID

        'добавляем все остальные объекты в случайном порядке
        Dim ObjectWeight(UBound(ActiveObjects)) As Double
        For i As Integer = 1 To UBound(ActiveObjects) Step 1
            If ActiveObjects(i).hasExternalGuard Then
                ObjectWeight(i) = 1 / (ActiveObjects(i).Size + 4) ^ 2
            Else
                ObjectWeight(i) = 1 / (ActiveObjects(i).Size + 2) ^ 2
            End If
        Next i
        Dim AllObjList(sum - p), s As Integer
        Dim Weight(sum - p) As Double
        Dim ids As New List(Of Integer)
        Dim t As Integer = 0
        For i As Integer = 0 To UBound(nObj) Step 1
            For j As Integer = 1 To nObj(i) Step 1
                AllObjList(t) = i
                Weight(t) = ObjectWeight(i)
                ids.Add(t)
                t += 1
            Next j
        Next i
        Do While ids.Count > 0
            s = comm.RandomSelection(ids, Weight, True)
            ids.Remove(s)
            Call AddObjId(places, nObj, AllObjList(s), p, areaUsed)
            If areaUsed >= LocArea(1) Then
                ReDim Preserve places(p - 1)
                Call SetNearWithSettings(places, loc, map)
                Exit Sub
            End If
        Loop
        Call SetNearWithSettings(places, loc, map)
    End Sub
    Private Sub SetNearWithSettings(ByRef places() As ActiveObjectsPlacer.ObjectPlacingSettings, _
                                    ByRef loc As Location, ByRef m As Map)
        Dim capitalN As Integer = -1
        For i As Integer = 0 To UBound(places) Step 1
            places(i).placeNearWith = -1
            places(i).sigma = 0
        Next i
        For i As Integer = 0 To UBound(places) Step 1
            If places(i).objectType = DefMapObjects.Types.Capital Then
                capitalN = i
                places(i).SetDistanceSettings(DefMapObjects.Types.None, ActiveObjects, rndgen)
                Exit For
            End If
        Next i
        If capitalN > -1 Then
            Dim n As Integer = 0
            For i As Integer = 0 To UBound(places) Step 1
                If places(i).objectType = DefMapObjects.Types.Mine Then
                    places(i).placeNearWith = capitalN
                    places(i).SetDistanceSettings(places(places(i).placeNearWith).objectType, ActiveObjects, rndgen)
                    n += 1
                    If n = 2 Then Exit For
                End If
            Next i
        End If
        For i As Integer = 0 To UBound(places) Step 1
            If places(i).objectType = DefMapObjects.Types.City Then
                For j As Integer = 0 To UBound(places) Step 1
                    If places(j).objectType = DefMapObjects.Types.Mine And places(j).placeNearWith = -1 Then
                        places(j).placeNearWith = i
                        places(j).SetDistanceSettings(places(places(j).placeNearWith).objectType, ActiveObjects, rndgen)
                        Exit For
                    End If
                Next j
            End If
        Next i
        'сортируем так, чтобы те, что должны быть рядом с чем-то расположены, сразу шли после этого чего-то
        Dim dependentObjects(UBound(places)) As List(Of ActiveObjectsPlacer.ObjectPlacingSettings)
        Dim newPlaces(UBound(places)) As ActiveObjectsPlacer.ObjectPlacingSettings
        For i As Integer = 0 To UBound(places) Step 1
            dependentObjects(i) = New List(Of ActiveObjectsPlacer.ObjectPlacingSettings)
        Next i
        For i As Integer = 0 To UBound(places) Step 1
            If places(i).placeNearWith > -1 Then
                dependentObjects(places(i).placeNearWith).Add(ActiveObjectsPlacer.ObjectPlacingSettings.Copy(places(i)))
            End If
        Next i
        Dim k As Integer = -1
        Dim newNearWith As Integer
        For i As Integer = 0 To UBound(places) Step 1
            If places(i).placeNearWith = -1 Then
                k += 1
                newPlaces(k) = ActiveObjectsPlacer.ObjectPlacingSettings.Copy(places(i))
                If dependentObjects(i).Count > 0 Then
                    newNearWith = k
                    For Each p As ActiveObjectsPlacer.ObjectPlacingSettings In dependentObjects(i)
                        k += 1
                        newPlaces(k) = ActiveObjectsPlacer.ObjectPlacingSettings.Copy(p)
                        newPlaces(k).placeNearWith = newNearWith
                    Next p
                End If
            End If
        Next i
        places = newPlaces
        If capitalN > -1 And m.symmID = -1 Then
            Dim n As Integer
            Dim area(UBound(places)), areaSum As Integer
            For i As Integer = 0 To UBound(places) Step 1
                If Not places(i).objectType = DefMapObjects.Types.Capital And places(i).placeNearWith = -1 Then
                    n += 1
                    area(i) = CInt(ActiveObjects(places(i).objectType).Size ^ 2)
                    For j As Integer = 0 To UBound(places) Step 1
                        If places(j).placeNearWith = i Then
                            area(i) = CInt(ActiveObjects(places(j).objectType).Size ^ 2)
                        End If
                    Next j
                    areaSum += area(i)
                End If
            Next i
            If n > 0 Then
                Dim Rnext, Rprev As Double
                Dim R0 As Double = Math.Sqrt(0.5) * ActiveObjects(DefMapObjects.Types.Capital).Size + 1
                Dim R1 As Double = 0.5 * Math.Min(loc.gASize, loc.gBSize)
                Dim R2 As Double = 0.5 * Math.Max(loc.gASize, loc.gBSize)
                Dim Area1 As Double = R1 ^ 2 - R0 ^ 2
                Dim Area2 As Double = R1 * (R2 - R1)
                Rprev = R0
                Dim W1 As Double = areaSum * Area1 / (Area1 + Area2)
                For i As Integer = 0 To UBound(places) Step 1
                    If Not places(i).objectType = DefMapObjects.Types.Capital And places(i).placeNearWith = -1 Then
                        places(i).placeNearWith = capitalN
                        places(i).applyUniformity = True
                        places(i).sigma = 1.5

                        places(i).SetDistanceSettings(places(places(i).placeNearWith).objectType, ActiveObjects, rndgen)

                        'R_prev < R1
                        ' (R_next^2 - R_prev^2)/(Area1+Area2) =area(i)/areasum 
                        ' if R_next>R1
                        '   ((R1^2 - R_prev^2)+(R_next - R_1)*R1)/(Area1+Area2) =area(i)/areasum    
                        ' end if
                        'else
                        ' (R_next - R_prev)*R1/(Area1+Area2) =area(i)/areasum 
                        'end if

                        If Rprev < R1 Then
                            Rnext = Math.Sqrt(area(i) * (Area1 + Area2) / areaSum + Rprev ^ 2)
                            If Rnext > R1 Then
                                Rnext = (area(i) * (Area1 + Area2) / areaSum - R1 ^ 2 + Rprev ^ 2) / R1 + R1
                            End If
                        Else
                            Rnext = area(i) * (Area1 + Area2) / (areaSum * R1) + Rprev
                        End If

                        places(i).prefferedDistance += 0.5 * (Rnext + Rprev)
                        Rprev = Rnext
                    End If
                Next i
            End If
        End If
    End Sub
    Private Function DesiredDist(ByRef objType1 As Integer, ByRef objType2 As Integer) As Double
        Return 0.5 * rndgen.Rand(1.05, Math.Sqrt(2.1), True) * (ActiveObjects(objType1).Size + ActiveObjects(objType2).Size) + rndgen.Rand(0, 3, True)
    End Function

    Private Sub AddObjId(ByRef places() As ActiveObjectsPlacer.ObjectPlacingSettings, _
                         ByRef nObj() As Integer, ByRef id As Integer, _
                         ByRef p As Integer, ByRef AreaUsed As Integer)
        places(p).objectType = ActiveObjects(id).TypeID
        AreaUsed = CInt(AreaUsed + (ActiveObjects(id).Area * 1.2))
        If Not IsNothing(nObj) Then nObj(id) -= 1
        p += 1
    End Sub
    Private Function FillLocation(ByVal LocId As Integer, ByRef m As Map, ByRef LocsPlacing() As Location.Borders, _
                                  ByRef LocArea()() As Integer, ByVal settMap As Map.SettingsMap, ByVal settLoc() As Map.SettingsLoc, _
                                  ByVal symmId As Integer, ByVal IsRaceLoc As Boolean, LocSymmMult() As Double, _
                                  ByRef LocFreeCells()(,) As Boolean, ByVal maxTime As Long) As ObjectPlacingResult
        Dim tmpm As Map = m
        Dim tmpLocsPlacing() As Location.Borders = LocsPlacing
        Dim tmpLocFreeCells()(,) As Boolean = LocFreeCells
        Dim TT As New TerminationCondition(maxTime)
        Dim places()() As ActiveObjectsPlacer.ObjectPlacingSettings = Nothing
        Dim ok As Boolean = False
        Dim msg As String = ""
        Dim v()() As Point = Nothing
        Dim CapitalPrefferedPositions() As Point = Nothing
        Do While Not ok
            Call TT.CheckTime()
            If TT.ExitFromLoops Then Exit Do

            CapitalPrefferedPositions = ActiveObjectsPlacer.DefineCapitalPreferedPos(settMap, LocFreeCells, ActiveObjects)
            v = Nothing
            If symmId > -1 Or Not IsRaceLoc Then
                ReDim v(0), places(0)
                Call MakeLocObjectsList(places(0), m.Loc(LocId - 1), settLoc(LocId - 1), IsRaceLoc, LocArea(LocId - 1), LocSymmMult(LocId - 1), tmpm)
                Call ObjectsPlacingVariants(places(0), LocId, tmpm, settMap, tmpLocsPlacing, _
                                            tmpLocFreeCells(LocId - 1), CapitalPrefferedPositions(LocId - 1), _
                                            v(0), CLng(Math.Max(1000, maxTime / 3)))
            Else
                ReDim v(settMap.nRaces - 1), places(settMap.nRaces - 1)
                For i As Integer = 0 To settMap.nRaces - 1 Step 1
                    Call MakeLocObjectsList(places(i), m.Loc(LocId - 1), settLoc(i), IsRaceLoc, LocArea(i), LocSymmMult(i), tmpm)
                Next i
                Parallel.For(0, settMap.nRaces, _
                 Sub(i As Integer)
                     Call ObjectsPlacingVariants(places(i), i + 1, tmpm, settMap, tmpLocsPlacing, _
                                                 tmpLocFreeCells(i), CapitalPrefferedPositions(LocId - 1), _
                                                 v(i), CLng(Math.Max(1000, maxTime / 3)))
                 End Sub)
            End If
            'If TT.ExitFromLoops Then
            '    Term = TT
            '    Exit Do
            'End If
            '#######################
            'ТЕСТ ПОЛУЧИВШЕЙСЯ РАССТАНОВКИ
            '#######################
            Dim minN As Integer = Integer.MaxValue
            For i As Integer = 0 To UBound(v) Step 1
                If IsNothing(v(i)) Then
                    If IsRaceLoc Then
                        msg = "Как минимум одна из стартовых локаций настолько маленькая, что я не могу разместить даже столицу"
                        GoTo exitfunction
                    Else
                        Dim sL As Map.SettingsLoc = settLoc(LocId - 1)
                        If sL.maxCities + sL.maxGoldMines + sL.maxManaSources _
                         + sL.maxMages + sL.maxMercenaries + sL.maxRuins + sL.maxTrainers + sL.maxVendors > 1 Then
                            'msg = "Не получилось ничего вместить в локацию"
                            GoTo exitfunction
                        Else
                            Exit Do
                        End If
                    End If
                End If
                If minN > UBound(v(i)) Then minN = UBound(v(i))
            Next i
            If Not symmId > -1 And IsRaceLoc Then
                For i As Integer = 0 To UBound(v) Step 1
                    If minN < UBound(v(i)) Then ReDim Preserve v(i)(minN)
                Next i
            End If
            ok = True
            Dim minRmetric, maxRmetric, Rsum As Double
            minRmetric = Double.MaxValue
            maxRmetric = Double.MinValue
            Dim tolerance As Double
            For i As Integer = 0 To UBound(v) Step 1
                tolerance = 0.333 * (Math.Pow(1 + settLoc(i + LocId - 1).maxRadiusDispersion, 2) + _
                         Math.Pow(1 + settLoc(i + LocId - 1).maxEccentricityDispersion, 2) + 1.2 ^ 2)
                Rsum = 0
                For n As Integer = 1 To UBound(v(i)) Step 1
                    Rsum += v(i)(n).SqDist(v(i)(0))
                Next n
                minRmetric = Math.Min(minRmetric, (Rsum * Math.Sqrt(tolerance)) / (v(i).Length ^ 2))
                maxRmetric = Math.Max(maxRmetric, (Rsum / Math.Sqrt(tolerance)) / (v(i).Length ^ 2))
            Next i
            If minRmetric < maxRmetric Then ok = False


            '#######################
            'РАССТАНОВКА ОБЪЕКТОВ
            '#######################
            'If ok Then
            '    Dim ex As Boolean = False
            '    Dim tmp_G As Integer = GroupID
            '    Parallel.For(0, v.Length, _
            '     Sub(i As Integer)
            '         Dim lensum As Integer = 0
            '         Dim g As Integer = tmp_G
            '         For j As Integer = 0 To i - 1 Step 1
            '             g += v(i).Length
            '         Next j
            '         Dim nPlacedCities As Integer = 0
            '         'Dim g As Integer = tmp_G + i * (minN + 1)
            '         For n As Integer = 0 To UBound(v(i)) Step 1
            '             Call ActiveObjectsPlacer.PlaceObject(tmpm, places(i)(n).objectType, v(i)(n).X, v(i)(n).Y, _
            '                                                  g + n, nPlacedCities, settLoc(i + LocId - 1), settMap, _
            '                                                  ActiveObjects, symm)
            '         Next n
            '         If Not IsNothing(settLoc(i + LocId - 1).RaceCities) AndAlso
            '          nPlacedCities < settLoc(i + LocId - 1).RaceCities.Length Then
            '             ex = True
            '         End If
            '     End Sub)
            '    For i As Integer = 0 To UBound(v) Step 1
            '        GroupID += v(i).Length
            '    Next i
            '    If ex Then Throw New Exception("Одна из локаций слишком маленькая, чтобы вместить города, обязательные для добавления")
            '    'GroupID += v.Length * (minN + 1)
            'End If
        Loop
exitfunction:
        m = tmpm
        Dim res As ObjectPlacingResult
        res.msg = msg
        res.ok = ok
        res.positions = v
        res.places = places
        res.LocID = LocId

        Return res
    End Function
    Friend Shared Function GetMaxGroupID(ByRef m As Map) As Integer
        Dim maxGroupID As Integer = -1
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                maxGroupID = Math.Max(maxGroupID, m.board(x, y).groupID)
            Next x
        Next y
        Return maxGroupID
    End Function
    Private Sub PlaceSelectedObjects(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc, _
                                     ByRef objects As ObjectPlacingResult, ByRef GroupID As Integer)
        If Not objects.ok Then Exit Sub
        For i As Integer = 0 To UBound(objects.positions) Step 1
            Dim lensum As Integer = 0
            Dim nPlacedCities As Integer = 0
            Dim locID As Integer = i + objects.LocID - 1
            'Dim g As Integer = tmp_G + i * (minN + 1)
            For n As Integer = 0 To UBound(objects.positions(i)) Step 1
                GroupID += 1
                Call ActiveObjectsPlacer.PlaceObject(m, objects.places(i)(n).objectType, _
                                                     objects.positions(i)(n).X, objects.positions(i)(n).Y, _
                                                     GroupID, nPlacedCities, settLoc(locID), settMap, _
                                                     ActiveObjects, symm)
            Next n
            If Not IsNothing(settLoc(locID).RaceCities) AndAlso
             nPlacedCities < settLoc(locID).RaceCities.Length Then
                Throw New Exception("Одна из локаций слишком маленькая, чтобы вместить города, обязательные для добавления")
            End If
        Next i
        'For i As Integer = 0 To UBound(v) Step 1
        '    GroupID += v(i).Length
        'Next i

        'GroupID += v.Length * (minN + 1)
    End Sub
    Private Structure ObjectPlacingResult
        Dim msg As String
        Dim places()() As ActiveObjectsPlacer.ObjectPlacingSettings
        Dim positions()() As Point
        Dim LocID As Integer
        Dim ok As Boolean
    End Structure
#End Region

#Region "Add impenetrable tiles inside locations"
    ''' <summary>Запускаем сразу после PlaceActiveObjects. После выполнения идем как в примере</summary>
    Public Sub MakeLabyrinth(ByRef m As Map, ByVal settMap As Map.SettingsMap, ByVal settLoc() As Map.SettingsLoc, _
                             ByRef Term As TerminationCondition)
        If Term.ExitFromLoops Then Exit Sub
        Dim tmpm As Map = m
        Dim TT(UBound(tmpm.Loc)) As TerminationCondition
        Dim maxTime As Long = Term.maxTime
        Dim ex(UBound(tmpm.Loc)) As String
        Parallel.For(0, tmpm.Loc.Length, _
         Sub(i As Integer)
             If Not tmpm.Loc(i).IsObtainedBySymmery Then
                 TT(i) = New TerminationCondition(maxTime)
                 ex(i) = MakeLabyrinth(tmpm, settMap, settLoc, tmpm.Loc(i).ID, TT(i))
             End If
         End Sub)
        For i As Integer = 0 To UBound(tmpm.Loc) Step 1
            If Not ex(i) = "" Then Throw New Exception(ex(i))
            If Not IsNothing(TT(i)) Then Term.ExitFromLoops = Term.ExitFromLoops Or TT(i).ExitFromLoops
        Next i
        If Term.ExitFromLoops Then Exit Sub
        Term = New TerminationCondition(Term.maxTime)
        Dim sm As New Map.SettingsMap With {.ApplySymmetry = settMap.ApplySymmetry, _
                                            .minPassWidth = 1.1, _
                                            .nRaces = settMap.nRaces, _
                                            .SymmetryClass = settMap.SymmetryClass, _
                                            .xSize = settMap.xSize, _
                                            .ySize = settMap.ySize}
        Call ConnectDisconnectedAreas(tmpm, sm, Term)
        m = tmpm
    End Sub
    Private Function MakeLabyrinth(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc, _
                                   ByRef LocId As Integer, ByRef Term As TerminationCondition) As String
        Try
            Dim b As New Location.Borders With {.minX = Integer.MaxValue, .minY = Integer.MaxValue, _
                                                .maxX = Integer.MinValue, .maxY = Integer.MinValue}
            Dim n As Location.Borders
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If m.board(x, y).locID(0) = LocId Then
                        b.minX = Math.Min(b.minX, x)
                        b.minY = Math.Min(b.minY, y)
                        b.maxX = Math.Max(b.maxX, x)
                        b.maxY = Math.Max(b.maxY, y)
                        m.board(x, y).passability.isPass = False
                        If Not m.board(x, y).passability.isBorder And Not m.board(x, y).passability.isAttended Then
                            n = NearestXY(x, y, m.xSize, m.ySize, 1)
                            For j As Integer = n.minY To n.maxY Step 1
                                For i As Integer = n.minX To n.maxX Step 1
                                    If Not m.board(i, j).locID(0) = LocId Then
                                        m.board(x, y).passability.isPenetrable = True
                                        m.board(x, y).passability.isPass = True
                                        i = n.maxX
                                        j = n.maxY
                                    End If
                                Next i
                            Next j
                        End If
                    End If
                Next x
            Next y
            For y As Integer = b.minY To b.maxY Step 1
                For x As Integer = b.minX To b.maxX Step 1
                    If m.board(x, y).locID(0) = LocId Then
                        m.board(x, y).passability.isPass = m.board(x, y).passability.isPass And m.board(x, y).passability.isPenetrable
                        If m.symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
                            For k As Integer = 0 To UBound(p) Step 1
                                m.board(p(k).X, p(k).Y).passability.isPass = m.board(x, y).passability.isPass
                            Next k
                        End If
                    End If
                Next x
            Next y

            Dim free(b.maxX - b.minX, b.maxY - b.minY) As Boolean
            For y As Integer = 0 To b.maxY - b.minY Step 1
                For x As Integer = 0 To b.maxX - b.minX Step 1
                    If m.board(x + b.minX, y + b.minY).locID(0) = LocId _
                    And Not m.board(x + b.minX, y + b.minY).passability.isAttended _
                    And Not m.board(x + b.minX, y + b.minY).passability.isBorder Then
                        free(x, y) = True
                    End If
                Next x
            Next y
            Dim init() As Point = Nothing
            Dim conn()(,) As Boolean = Nothing
            Dim s As Boolean
            For y As Integer = 0 To b.maxY - b.minY Step 1
                For x As Integer = 0 To b.maxX - b.minX Step 1
                    If free(x, y) And m.board(x + b.minX, y + b.minY).passability.isPass Then
                        s = True
                        If Not IsNothing(conn) Then
                            For i As Integer = 0 To UBound(conn) Step 1
                                If conn(i)(x, y) Then
                                    s = False
                                    Exit For
                                End If
                            Next i
                        End If
                        If s Then
                            If IsNothing(init) Then
                                ReDim init(0), conn(0)
                            Else
                                ReDim Preserve init(init.Length), conn(conn.Length)
                            End If
                            init(UBound(init)) = New Point(x, y)
                            conn(UBound(conn)) = FindConnected(free, init(UBound(init)))
                        End If
                    End If
                Next x
            Next y
            If Not IsNothing(conn) Then
                If Not IsNothing(FindDisconnected(free, conn)) Then
                    Dim p As Point = FindDisconnected(free, conn)
                    Return "Какой-то объект перекрывает проход, чего не должно было получиться. Точка " & p.X + b.minX & vbTab & p.Y + b.minY
                End If

                For i As Integer = 0 To UBound(conn) Step 1
                    Call LifeAlgo(m, settMap, settLoc, conn(i), init(i), New Point(b.minX, b.minY), Term)
                Next i
            End If

            Dim setAsBorder(b.maxX - b.minX, b.maxY - b.minY) As Boolean
            Dim makeBorder As Boolean
            For y As Integer = b.minY To b.maxY Step 1
                For x As Integer = b.minX To b.maxX Step 1
                    If m.board(x, y).locID(0) = LocId And Not m.board(x, y).passability.isAttended _
                    And Not m.board(x, y).passability.isBorder And m.board(x, y).passability.isPass _
                    And Not m.board(x, y).stack.ObjectGuard And Not m.board(x, y).stack.PassGuardLoc And Not m.board(x, y).stack.GuardLoc Then
                        n = NearestXY(x, y, m.xSize, m.ySize, 1)
                        makeBorder = True
                        For j As Integer = n.minY To n.maxY Step 1
                            For i As Integer = n.minX To n.maxX Step 1
                                If m.board(i, j).passability.isBorder Then
                                    makeBorder = False
                                    i = n.maxX
                                    j = n.maxY
                                End If
                            Next i
                        Next j
                        If makeBorder Then If rndgen.Rand(0, 1) > 0.65 Then setAsBorder(x - b.minX, y - b.minY) = True
                    End If
                Next x
            Next y
            For y As Integer = b.minY To b.maxY Step 1
                For x As Integer = b.minX To b.maxX Step 1
                    If setAsBorder(x - b.minX, y - b.minY) Then
                        If m.symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
                            For k As Integer = 0 To UBound(p) Step 1
                                m.board(p(k).X, p(k).Y).passability.isPass = False
                                m.board(p(k).X, p(k).Y).passability.isPenetrable = False
                                m.board(p(k).X, p(k).Y).passability.isBorder = True
                            Next k
                        Else
                            m.board(x, y).passability.isPass = False
                            m.board(x, y).passability.isPenetrable = False
                            m.board(x, y).passability.isBorder = True
                        End If
                    End If
                Next x
            Next y
        Catch ex As Exception
            Return ex.Message
        End Try
        Return ""
    End Function
    Private Sub LifeAlgo(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc, _
                         ByRef connected(,) As Boolean, _
                         ByRef init As Point, ByRef LPos As Point, ByRef Term As TerminationCondition)

        Dim xSize As Integer = UBound(connected, 1)
        Dim ySize As Integer = UBound(connected, 2)
        Dim isLifeField(xSize, ySize) As Boolean

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If connected(x, y) And Not m.board(x + LPos.X, y + LPos.Y).passability.isPenetrable Then isLifeField(x, y) = True
            Next x
        Next y
        If m.symmID > -1 Then
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isLifeField(x, y) Then
                        Dim p() As Point = symm.ApplySymm(New Point(x + LPos.X, y + LPos.Y), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            Dim tx As Integer = p(k).X - LPos.X
                            Dim ty As Integer = p(k).Y - LPos.Y
                            If tx >= 0 And ty >= 0 And tx <= xSize And ty <= ySize Then isLifeField(tx, ty) = False
                        Next k
                        isLifeField(x, y) = True
                    End If
                Next x
            Next y
        End If

        Dim free(,) As Boolean = CType(connected.Clone, Boolean(,))
        Dim W(xSize, ySize) As Double
        Dim initChance As Double = 0.9
        Dim deathLower As Integer = 1
        Dim deatUpper As Integer = 3
        Dim birth As Integer = 2
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isLifeField(x, y) AndAlso rndgen.Rand(0, 1) < initChance Then free(x, y) = False
            Next x
        Next y

        Dim bW(8), dW(8), maxW As Double
        For i As Integer = 0 To UBound(bW) Step 1
            bW(i) = Math.Sqrt(1 + Math.Abs(i - birth) ^ 2)
            dW(i) = Math.Sqrt(4 + Math.Abs(deathLower - i) ^ 2 + Math.Abs(deatUpper - i) ^ 2)
            maxW = Math.Max(maxW, Math.Max(bW(i), dW(UBound(bW) - i)))
        Next i
        Dim nloops As Integer = 0
        Dim nextloop As Boolean = True
        Do While nextloop
            Term.CheckTime()
            If Term.ExitFromLoops Then Exit Sub
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isLifeField(x, y) Then
                        Dim b As Location.Borders = NearestXY(x, y, xSize, ySize, 1)
                        Dim ndead As Integer = 9 - (b.maxX - b.minX + 1) * (b.maxY - b.minY + 1)
                        If Not free(x, y) Then ndead -= 1
                        For j As Integer = b.minY To b.maxY Step 1
                            For i As Integer = b.minX To b.maxX Step 1
                                If Not connected(i, j) Or Not free(i, j) Then
                                    ndead += 1
                                End If
                            Next i
                        Next j
                        If free(x, y) Then
                            W(x, y) = bW(ndead)
                        Else
                            W(x, y) = dW(ndead)
                        End If
                    End If
                Next x
            Next y
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isLifeField(x, y) AndAlso W(x, y) > rndgen.Rand(0, maxW) Then
                        free(x, y) = Not free(x, y)
                    End If
                Next x
            Next y
            nloops += 1
            If nloops > 2 Then
                nextloop = False
                Dim tconn(,) As Boolean = FindConnected(free, init)
                nextloop = Not CompareConnection(free, connected, tconn)
            End If
        Loop

        For y As Integer = 1 To ySize - 1 Step 1
            For x As Integer = 1 To xSize - 1 Step 1
                W(x, y) = 0
                If isLifeField(x, y) And free(x, y) Then
                    Dim b As Location.Borders = NearestXY(x, y, xSize, ySize, 1)
                    Dim ndead As Integer = 0
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If Not connected(i, j) Or Not free(i, j) Then
                                ndead += 1
                            End If
                        Next i
                    Next j
                    If ndead < 1 And ndead < 5 Then
                        Dim t As Integer = 0
                        Dim i As Integer
                        Dim j As Integer
                        For k As Integer = -1 To 1 Step 2
                            i = x + k
                            j = y
                            If Not connected(i, j) Or Not free(i, j) Then t += 1
                            i = x
                            j = y + k
                            If Not connected(i, j) Or Not free(i, j) Then t += 1
                        Next k
                        If ndead = t Then W(x, y) = 0.3 + 0.1 * CDbl(ndead)
                    End If
                End If
            Next x
        Next y
        For y As Integer = 1 To ySize - 1 Step 1
            For x As Integer = 1 To xSize - 1 Step 1
                If W(x, y) > 0 AndAlso W(x, y) > rndgen.Rand(0, maxW) Then
                    free(x, y) = Not free(x, y)
                    Dim tconn(,) As Boolean = FindConnected(free, init)
                    If Not CompareConnection(free, connected, tconn) Then free(x, y) = Not free(x, y)
                End If
            Next x
        Next y

        'делаем карту свободнее в целом
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isLifeField(x, y) And Not free(x, y) Then
                    Dim decAmount As Double = settLoc(m.board(x + LPos.X, y + LPos.Y).locID(0) - 1).DecorationsAmount
                    If decAmount < 1 Then
                        If decAmount > 0 Then
                            If decAmount < rndgen.Rand(0, 1) Then free(x, y) = True
                        Else
                            free(x, y) = True
                        End If
                    End If
                End If
            Next x
        Next y

        'собираем в кучи непроходмые пиксели
        Dim lifeAlgoPoints As New List(Of String)
        Dim isInLifeAlgoPointsList(xSize, ySize) As Boolean
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isLifeField(x, y) Then
                    lifeAlgoPoints.Add(x & "_" & y)
                    isInLifeAlgoPointsList(x, y) = True
                End If
            Next x
        Next y
        Dim MinR As Double = 2
        Dim MaxR As Double = 4
        Dim excludeR2 As Double = (1.5 * MaxR) ^ 2
        Dim moveChance As Double = 0.2
        Do While lifeAlgoPoints.Count > 0
            Dim pstr() As String = lifeAlgoPoints.Item(rndgen.RndIntFast(0, lifeAlgoPoints.Count - 1)).Split(CChar("_"))
            Dim p As Point = New Point(CInt(pstr(0)), CInt(pstr(1)))
            Dim R2 As Double = rndgen.Rand(MinR, MaxR) ^ 2
            Dim nTries As Integer = CInt(1.5 * Math.Sqrt(R2) / Math.Min(moveChance, 1))
            Dim movingPoints((xSize + 1) * (ySize + 1) - 1) As Point
            Dim nMovingP As Integer = -1
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isInLifeAlgoPointsList(x, y) Then
                        If p.SqDist(x, y) < excludeR2 Then
                            isInLifeAlgoPointsList(x, y) = False
                            lifeAlgoPoints.Remove(x & "_" & y)
                        End If
                        If Not free(x, y) AndAlso p.SqDist(x, y) <= R2 Then
                            nMovingP += 1
                            movingPoints(nMovingP) = New Point(x, y)
                        End If
                    End If
                Next x
            Next y
            If nMovingP > -1 Then
                Dim nearestList As New List(Of String)
                ReDim Preserve movingPoints(nMovingP)
                For t As Integer = 0 To nTries Step 1
                    For n As Integer = 0 To nMovingP Step 1
                        If rndgen.Rand(0, 1) < moveChance Then
                            nearestList.Clear()
                            Dim d As Double = p.SqDist(movingPoints(n))
                            Dim b As Location.Borders = NearestXY(movingPoints(n).X, movingPoints(n).Y, xSize, ySize, 1)
                            For j As Integer = b.minY To b.maxY Step 1
                                For i As Integer = b.minX To b.maxX Step 1
                                    If free(i, j) And isLifeField(i, j) AndAlso p.SqDist(i, j) < d Then
                                        nearestList.Add(i & "_" & j)
                                    End If
                                Next i
                            Next j
                            If nearestList.Count > 0 Then
                                Dim selected() As String = nearestList.Item(rndgen.RndIntFast(0, nearestList.Count - 1)).Split(CChar("_"))
                                Dim xs As Integer = CInt(selected(0))
                                Dim ys As Integer = CInt(selected(1))
                                free(movingPoints(n).X, movingPoints(n).Y) = True
                                free(xs, ys) = False
                                movingPoints(n).X = xs
                                movingPoints(n).Y = ys
                            End If
                        End If
                    Next n
                Next t
            End If
        Loop

        'делаем карту рядом со стольней свободнее
        For j As Integer = 0 To ySize Step 1
            For i As Integer = 0 To xSize Step 1
                If m.board(i + LPos.X, j + LPos.Y).mapObject.objectID = DefMapObjects.Types.Capital Then
                    Dim center As New Point(CInt(i + (ActiveObjects(DefMapObjects.Types.Capital).Size - 1) * 0.5), _
                                            CInt(j + (ActiveObjects(DefMapObjects.Types.Capital).Size - 1) * 0.5))
                    Dim R As Double = 7
                    For y As Integer = 0 To ySize Step 1
                        For x As Integer = 0 To xSize Step 1
                            Dim d As Double = center.Dist(x, y)
                            If isLifeField(x, y) And d < R Then
                                Dim delChance As Double = settLoc(m.board(x + LPos.X, y + LPos.Y).locID(0) - 1).DecorationsAmount
                                delChance *= 0.75 * (R - d) / R
                                If delChance = 1 OrElse delChance > rndgen.Rand(0, 1) Then free(x, y) = True
                            End If
                        Next x
                    Next y
                End If
            Next i
        Next j

        'применяем результат к карте
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isLifeField(x, y) Then
                    If m.symmID > -1 Then
                        Dim p() As Point = symm.ApplySymm(New Point(x + LPos.X, y + LPos.Y), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            m.board(p(k).X, p(k).Y).passability.isBorder = Not free(x, y)
                        Next k
                    Else
                        m.board(x + LPos.X, y + LPos.Y).passability.isBorder = Not free(x, y)
                    End If
                End If
            Next x
        Next y

    End Sub
    Private Function CompareConnection(ByRef freeCells(,) As Boolean, ByRef connected1(,) As Boolean, _
                                       ByRef connected2(,) As Boolean) As Boolean
        Dim xSize As Integer = UBound(freeCells, 1)
        Dim ySize As Integer = UBound(freeCells, 2)
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If freeCells(x, y) And (Not connected1(x, y) = connected2(x, y)) Then Return False
            Next x
        Next y
        Return True
    End Function
#End Region
End Class

Public Class SymmetryOperations

    Public Enum TwoPlayersSymmetryModes
        ''' <summary>Поворот на 180 градусов вокруг оси, находящейся в центре карты</summary>
        L2 = 0
        ''' <summary>Отражение в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
        xM = 1
        ''' <summary>Отражение в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
        yM = 2
        ''' <summary>Отражение в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
        xy1M = 3
        ''' <summary>Отражение в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
        xy2M = 4
    End Enum
    Public Enum FourPlayersSymmetryModes
        ''' <summary>Поворот на 90 градусов вокруг оси, находящейся в центре карты</summary>
        L4 = 0
        ''' <summary>Отражение в плоскостях, параллельных осям Ox и Oy, и проходящих через центр карты</summary>
        xMyM = 1
        ''' <summary>Отражение в плоскостях, параллельных диагонали {x, y} и {-x, y}, и проходящих через центр карты. Только для квадратных карт</summary>
        xy1Mxy2M = 2
    End Enum

#Region "Rotation"
    ''' <summary>Возвращает точку, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(mapXSize - p.X, mapYSize - p.Y)
    End Function
    ''' <summary>Возвращает точку, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef p As Point, ByRef m As Map) As Point
        Return L2(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = Location.Copy(L)
        Call r.L2Rotation(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef p As Point, ByRef mapSize As Integer) As Point
        Return New Point(p.Y, mapSize - p.X)
    End Function
    ''' <summary>Возвращает точку, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef p As Point, ByRef m As Map) As Point
        Return L4(p, m.xSize)
    End Function
    ''' <summary>Возвращает локацию, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = Location.Copy(L)
        Call r.L4Rotation(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
#End Region
#Region "Reflection"
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(p.X, mapYSize - p.Y)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef p As Point, ByRef m As Map) As Point
        Return xM(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = Location.Copy(L)
        Call r.xReflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(mapXSize - p.X, p.Y)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef p As Point, ByRef m As Map) As Point
        Return yM(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = Location.Copy(L)
        Call r.yReflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef p As Point) As Point
        Return New Point(p.Y, p.X)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef p As Point, ByRef m As Map) As Point
        Return xy1M(p)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = Location.Copy(L)
        Call r.xy1Reflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef p As Point, ByRef mapSize As Integer) As Point
        Return New Point(mapSize - p.Y, mapSize - p.X)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef p As Point, ByRef m As Map) As Point
        Return xy2M(p, m.xSize)
    End Function
    ''' <summary>Возвращает локация, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = Location.Copy(L)
        Call r.xy2Reflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
#End Region

    ''' <summary>Применяет одну из операций симметрии, разрешенную для двух игроков</summary>
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function TwoPlayersSymm(ByRef p As Point, ByRef m As Map, _
                                    ByRef id As Integer, ByRef minSqDist As Integer) As Point()
        Dim pp As Point
        If id = TwoPlayersSymmetryModes.L2 Then
            pp = L2(p, m)
        ElseIf id = TwoPlayersSymmetryModes.xM Then
            pp = xM(p, m)
        ElseIf id = TwoPlayersSymmetryModes.yM Then
            pp = yM(p, m)
        ElseIf id = TwoPlayersSymmetryModes.xy1M Then
            pp = xy1M(p)
        ElseIf id = TwoPlayersSymmetryModes.xy2M Then
            pp = xy2M(p, m)
        Else
            Throw New Exception("Неожиданный id операции симметрии")
        End If
        Return CheckPointsDist(New Point() {p, pp}, minSqDist)
    End Function
    ''' <summary>Применяет одну из операций симметрии, разрешенную для двух игроков</summary>
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function TwoPlayersSymm(ByRef L As Location, ByRef m As Map, _
                                    ByRef id As Integer, ByRef minSqDist As Integer) As Location()
        Dim ll As Location
        If id = TwoPlayersSymmetryModes.L2 Then
            ll = L2(L, m)
        ElseIf id = TwoPlayersSymmetryModes.xM Then
            ll = xM(L, m)
        ElseIf id = TwoPlayersSymmetryModes.yM Then
            ll = yM(L, m)
        ElseIf id = TwoPlayersSymmetryModes.xy1M Then
            ll = xy1M(L, m)
        ElseIf id = TwoPlayersSymmetryModes.xy2M Then
            ll = xy2M(L, m)
        Else
            Throw New Exception("Неожиданный id операции симметрии")
        End If
        Return CheckPointsDist(New Location() {L, ll}, minSqDist)
    End Function
    Private Function CheckPointsDist(ByRef p() As Point, ByRef minSqDist As Integer) As Point()
        Dim dx, dy As Integer
        Dim ok As Boolean = True
        For i As Integer = 0 To UBound(p) - 1 Step 1
            For j As Integer = i + 1 To UBound(p) Step 1
                dx = p(i).X - p(j).X
                dy = p(i).Y - p(j).Y
                If dx * dx + dy * dy < minSqDist Then
                    ok = False
                    i = p.Length
                    Exit For
                End If
            Next j
        Next i
        If ok Then
            Return p
        Else
            Dim x, y As Integer
            For i As Integer = 0 To UBound(p) Step 1
                x += p(i).X
                y += p(i).Y
            Next i
            x = CInt(x / p.Length)
            y = CInt(y / p.Length)
            Return New Point() {New Point(x, y)}
        End If
    End Function
    Private Function CheckPointsDist(ByRef L() As Location, ByRef minSqDist As Integer) As Location()
        Dim dx, dy As Integer
        Dim ok As Boolean = True
        Dim minID As Integer = Integer.MaxValue
        For i As Integer = 0 To UBound(L) Step 1
            If minID > L(i).ID Then minID = L(i).ID
        Next i
        For i As Integer = 0 To UBound(L) - 1 Step 1
            For j As Integer = i + 1 To UBound(L) Step 1
                dx = L(i).pos.X - L(j).pos.X
                dy = L(i).pos.Y - L(j).pos.Y
                If dx * dx + dy * dy < minSqDist Then
                    ok = False
                    i = L.Length
                    Exit For
                End If
            Next j
        Next i
        If ok Then
            For i As Integer = 0 To UBound(L) Step 1
                L(i).ID = minID + i
            Next i
            Return L
        Else
            Dim x, y As Integer
            Dim a, b, alpha As Double
            Dim isSymm As Boolean = True
            For i As Integer = 0 To UBound(L) Step 1
                x += L(i).pos.X
                y += L(i).pos.Y
                a += L(i).gASize
                b += L(i).gBSize
                alpha += L(i).gAlpha
                If Not L(i).IsObtainedBySymmery Then isSymm = False
            Next i
            x = CInt(x / L.Length)
            y = CInt(y / L.Length)
            a /= L.Length
            b /= L.Length
            alpha /= L.Length
            Dim res As New Location(New Point(x, y), a, b, alpha, minID, isSymm)
            Return New Location() {res}
        End If
    End Function

    ''' <summary>Применяет одну из операций симметрии, разрешенную для четырех игроков</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function FourPlayersSymm(ByRef p As Point, ByRef m As Map, ByRef minSqDist As Integer) As Point()

        Dim res() As Point
        Dim n As Integer = -1
        If m.symmID = FourPlayersSymmetryModes.L4 Then
            ReDim res(3)
            res(0) = New Point(p.X, p.Y)
            For i As Integer = 1 To UBound(res) Step 1
                res(i) = L4(res(i - 1), m)
            Next i
            Return CheckPointsDist(res, minSqDist)
        ElseIf m.symmID = FourPlayersSymmetryModes.xMyM Or m.symmID = FourPlayersSymmetryModes.xy1Mxy2M Then
            Dim op1, op2 As Integer
            If m.symmID = FourPlayersSymmetryModes.xMyM Then
                op1 = TwoPlayersSymmetryModes.xM
                op2 = TwoPlayersSymmetryModes.yM
            Else
                op1 = TwoPlayersSymmetryModes.xy1M
                op2 = TwoPlayersSymmetryModes.xy2M
            End If
            Dim pp() As Point = TwoPlayersSymm(p, m, op1, minSqDist)
            Dim r(UBound(pp))() As Point
            For i As Integer = 0 To UBound(pp) Step 1
                r(i) = TwoPlayersSymm(pp(i), m, op2, minSqDist)
                n += r(i).Length
            Next i
            ReDim res(n)
            n = -1
            For i As Integer = 0 To UBound(r) Step 1
                For j As Integer = 0 To UBound(r(i)) Step 1
                    n += 1
                    res(n) = r(i)(j)
                Next j
            Next i
            Return res
        Else
            Throw New Exception("Неожиданный id операции симметрии")
            Return Nothing
        End If
    End Function
    ''' <summary>Применяет одну из операций симметрии, разрешенную для четырех игроков</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function FourPlayersSymm(ByRef L As Location, ByRef m As Map, ByRef minSqDist As Integer) As Location()

        Dim res() As Location
        Dim n As Integer = -1
        If m.symmID = FourPlayersSymmetryModes.L4 Then
            ReDim res(3)
            res(0) = Location.Copy(L)
            For i As Integer = 1 To UBound(res) Step 1
                res(i) = L4(res(i - 1), m)
            Next i
            Return CheckPointsDist(res, minSqDist)
        ElseIf m.symmID = FourPlayersSymmetryModes.xMyM Or m.symmID = FourPlayersSymmetryModes.xy1Mxy2M Then
            Dim op1, op2 As Integer
            If m.symmID = FourPlayersSymmetryModes.xMyM Then
                op1 = TwoPlayersSymmetryModes.xM
                op2 = TwoPlayersSymmetryModes.yM
            Else
                op1 = TwoPlayersSymmetryModes.xy1M
                op2 = TwoPlayersSymmetryModes.xy2M
            End If
            Dim ll() As Location = TwoPlayersSymm(L, m, op1, minSqDist)
            Dim r(UBound(ll))() As Location
            For i As Integer = 0 To UBound(ll) Step 1
                r(i) = TwoPlayersSymm(ll(i), m, op2, minSqDist)
                n += r(i).Length
            Next i
            ReDim res(n)
            n = -1
            For i As Integer = 0 To UBound(r) Step 1
                For j As Integer = 0 To UBound(r(i)) Step 1
                    n += 1
                    res(n) = r(i)(j)
                Next j
            Next i
            Return CheckPointsDist(res, minSqDist)
        Else
            Throw New Exception("Неожиданный id операции симметрии")
            Return Nothing
        End If
    End Function

    ''' <summary>Возвращает список возможных операций симметрии</summary>
    ''' <param name="nRaces">Количество рас</param>
    ''' <param name="xSize">Размер по оси X</param>
    ''' <param name="ySize">Размер по оси Y</param>
    Public Function PossibleOperationsList(ByRef nRaces As Integer, ByRef xSize As Integer, ByRef ySize As Integer) As List(Of Integer)
        Dim res As New List(Of Integer)
        If nRaces = 2 Then
            res.AddRange(New Integer() {TwoPlayersSymmetryModes.L2, _
                                        TwoPlayersSymmetryModes.L2,
                                        TwoPlayersSymmetryModes.xM,
                                        TwoPlayersSymmetryModes.yM})
            If xSize = ySize Then res.AddRange(New Integer() {TwoPlayersSymmetryModes.xy1M, _
                                                              TwoPlayersSymmetryModes.xy2M})
        ElseIf nRaces = 4 Then
            res.Add(FourPlayersSymmetryModes.xMyM)
            If xSize = ySize Then res.AddRange(New Integer() {FourPlayersSymmetryModes.L4, _
                                                              FourPlayersSymmetryModes.xy1Mxy2M})
        End If
        Return res
    End Function
    ''' <summary>Возвращает список возможных операций симметрии</summary>
    ''' <param name="nRaces">Количество рас</param>
    ''' <param name="m">Карта. Важен только размер</param>
    Public Function PossibleOperationsList(ByRef nRaces As Integer, ByRef m As Map) As List(Of Integer)
        Return PossibleOperationsList(nRaces, m.xSize, m.ySize)
    End Function

    ''' <summary>Применяет одну из операций симметрии. ID симметрии в переменной m</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Public Function ApplySymm(ByRef p As Point, ByRef nRaces As Integer, ByRef m As Map, _
                              ByRef minSqDist As Integer) As Point()
        If nRaces = 2 Then
            Return TwoPlayersSymm(p, m, m.symmID, minSqDist)
        ElseIf nRaces = 4 Then
            Return FourPlayersSymm(p, m, minSqDist)
        Else
            Return Nothing
        End If
    End Function

    ''' <summary>Применяет одну из операций симметрии. ID симметрии в переменной m</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Public Function ApplySymm(ByRef L As Location, ByRef nRaces As Integer, ByRef m As Map, _
                              ByRef minSqDist As Integer) As Location()
        If nRaces = 2 Then
            Return TwoPlayersSymm(L, m, m.symmID, minSqDist)
        ElseIf nRaces = 4 Then
            Return FourPlayersSymm(L, m, minSqDist)
        Else
            Return Nothing
        End If
    End Function
End Class

Public Class Location

    ''' <summary>Базовая раса локации</summary>
    Public Race As Integer

    ''' <summary>Номер локации, больше ноля</summary>
    Public ID As Integer
    ''' <summary>Положение локации</summary>
    Public pos As Point
    Private invSqA, invSqB, cos, sin As Double
    Private Asize, Bsize As Double
    Private invSigmaA, invSigmaB As Double
    Private alpha As Double
    ''' <summary>True, если получена операцией симметрии</summary>
    Public IsObtainedBySymmery As Boolean

    Friend Structure Borders
        Dim maxX, minX, maxY, minY As Integer
    End Structure

    ''' <summary>Возвращает угол поворота локации</summary>
    Public Function gAlpha() As Double
        Return alpha
    End Function
    ''' <summary>Возвращает длину оси А</summary>
    Public Function gASize() As Double
        Return Asize
    End Function
    ''' <summary>Возвращает длину оси B</summary>
    Public Function gBSize() As Double
        Return Bsize
    End Function

    ''' <param name="p">Положение локации</param>
    ''' <param name="a">Половина ширины (если локация создается вне моего генератора, то можно ставить любое число > 0)</param>
    ''' <param name="b">Половина высоты (если локация создается вне моего генератора, то можно ставить любое число > 0)</param>
    ''' <param name="angle">Угол наклона от 0 до Пи (если локация создается вне моего генератора, то можно ставить любое число)</param>
    ''' <param name="i">Номер локации, больше ноля</param>
    ''' <param name="symmetred">Получено ли положение локации с помощью операции симметрии</param>
    Public Sub New(ByRef p As Point, ByVal a As Double, ByVal b As Double, ByVal angle As Double, ByVal i As Integer, Optional ByVal symmetred As Boolean = False)
        pos = New Point(p.X, p.Y)
        invSigmaA = Math.Sqrt(0.5) * 0.5 / a
        invSigmaB = Math.Sqrt(0.5) * 0.5 / b
        invSqA = 1 / (a * a)
        invSqB = 1 / (b * b)
        Asize = a
        Bsize = b
        ID = i
        alpha = angle
        Call CosSinCalc()
        IsObtainedBySymmery = symmetred
    End Sub

    Public Shared Function Copy(ByRef L As Location) As Location
        Return New Location(L.pos, L.gASize, L.gBSize, L.gAlpha, L.ID, L.IsObtainedBySymmery) With {.Race = L.Race}
    End Function

    Friend Shared Function FindLocIDByPosition(ByRef m As Map, ByRef p As Point) As Integer
        For j As Integer = 0 To UBound(m.Loc) Step 1
            If Math.Abs(m.Loc(j).pos.X - p.X) < 2 And Math.Abs(m.Loc(j).pos.Y - p.Y) < 2 Then Return j
            If j = UBound(m.Loc) Then Throw New Exception("Не могу найти локацию по координате")
        Next j
        Return -1
    End Function

    Private Sub CosSinCalc()
        cos = Math.Cos(alpha)
        sin = Math.Sin(alpha)
    End Sub

    Friend Sub L2Rotation(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha += Math.PI
        If alpha >= 2 * Math.PI Then alpha -= 2 * Math.PI
        Call CosSinCalc()
        pos = symm.L2(pos, m)
    End Sub
    Friend Sub L4Rotation(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha += 0.5 * Math.PI
        If alpha >= 2 * Math.PI Then alpha -= 2 * Math.PI
        Call CosSinCalc()
        pos = symm.L4(pos, m)
    End Sub
    Friend Sub xReflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = 2 * Math.PI - alpha
        Call CosSinCalc()
        pos = symm.xM(pos, m)
    End Sub
    Friend Sub yReflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.yM(pos, m)
    End Sub
    Friend Sub xy1Reflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = 0.5 * Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.xy1M(pos, m)
    End Sub
    Friend Sub xy2Reflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = 1.5 * Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.xy2M(pos, m)
    End Sub

    Friend Function IsInside(ByRef X As Integer, ByRef Y As Integer) As Boolean
        Dim dx As Integer = X - pos.X
        Dim dy As Integer = Y - pos.Y
        Dim x1 As Double = dx * cos
        Dim x2 As Double = dy * sin
        Dim y1 As Double = -dx * sin
        Dim y2 As Double = dy * cos
        x1 += x2
        y1 += y2
        If x1 * x1 * invSqA + y1 * y1 * invSqB > 1 Then
            Return False
        Else
            Return True
        End If
    End Function
    Friend Function IsInside(ByRef P As Point) As Boolean
        Return IsInside(P.X, P.Y)
    End Function

    Friend Function XYborders(ByRef maxX As Integer, ByRef maxY As Integer) As Borders
        Return XYborders(maxX, maxY, 0, 0)
    End Function
    Friend Function XYborders(ByRef maxX As Integer, ByRef maxY As Integer, ByRef minX As Integer, ByRef minY As Integer) As Borders
        Dim dx As Integer = CInt(Math.Ceiling(Math.Abs(Asize * cos) + Math.Abs(Bsize * sin)))
        Dim dy As Integer = CInt(Math.Ceiling(Math.Abs(Asize * sin) + Math.Abs(Bsize * cos)))
        Return New Borders With {.minX = Math.Max(pos.X - dx, minX), _
                                 .maxX = Math.Min(pos.X + dx, maxX), _
                                 .miny = Math.Max(pos.Y - dy, minX), _
                                 .maxy = Math.Min(pos.Y + dy, maxY)}
    End Function

    Friend Function pWeight(ByRef X As Integer, ByRef Y As Integer) As Double
        Dim dx As Integer = X - pos.X
        Dim dy As Integer = Y - pos.Y
        Dim d1 As Double = dx * cos + dy * sin
        Dim d2 As Double = -dx * sin + dy * cos
        Return Math.Exp(-((dx * invSigmaA) ^ 2) - ((dy * invSigmaB) ^ 2))
    End Function
    Friend Function pWeight(ByRef P As Point) As Double
        Return pWeight(P.X, P.Y)
    End Function
    Private Function Gauss(ByRef dX As Double, ByRef sigma As Double) As Double
        Return Math.Exp(-0.5 * (dX / sigma) ^ 2)
    End Function

    Friend Shared Function GenLocSize(ByRef sett As Map.SettingsLoc, ByRef id As Integer, _
                                      ByRef rndgen As RndValueGen, ByRef minLocationRadiusAtAll As Double) As Location
        Dim r, e, a As Double
        If sett.maxRadiusDispersion >= 1 Or sett.maxRadiusDispersion < 0 Then Throw New Exception("Invalid radius dispersion: " & sett.maxRadiusDispersion & "(>=1 or <0)")
        If sett.maxEccentricityDispersion >= 1 Or sett.maxEccentricityDispersion < 0 Then Throw New Exception("Invalid eccentricity dispersion: " & sett.maxEccentricityDispersion & "(>=1 or <0)")

        r = rndgen.PRand(1 - sett.maxRadiusDispersion, 1 + sett.maxRadiusDispersion) * sett.AverageRadius
        e = rndgen.PRand(1 - sett.maxEccentricityDispersion, 1 + sett.maxEccentricityDispersion)
        a = rndgen.PRand(0, Math.PI)
        r = Math.Max(r, minLocationRadiusAtAll)
        Return New Location(New Point(0, 0), r * e, r / e, a, id)
    End Function

End Class

Public Class ArrayZoom

    ''' <summary>Увеличит массив в multiplicator раз</summary>
    Public Function Zoom(ByVal grid(,) As Integer, ByVal multiplicator As Integer) As Integer(,)
        Dim imgXSize As Integer = UBound(grid, 1)
        Dim imgYSize As Integer = UBound(grid, 2)
        Dim res((imgXSize + 1) * multiplicator - 1, (imgYSize + 1) * multiplicator - 1) As Integer
        Parallel.For(0, imgXSize + 1, _
         Sub(i As Integer)
             Dim x, y, xm, ym As Integer
             xm = i * multiplicator
             For J As Integer = 0 To imgYSize Step 1
                 ym = J * multiplicator
                 For i1 As Integer = 0 To multiplicator - 1 Step 1
                     x = xm + i1
                     For j1 As Integer = 0 To multiplicator - 1 Step 1
                         y = ym + j1
                         res(x, y) = grid(i, J)
                     Next j1
                 Next i1
             Next J
         End Sub)
        Return res
    End Function

    ''' <summary>Определит, во сколько раз нужно увеличить массив</summary>
    ''' <param name="size">max(ширина,высота)</param>
    ''' <param name="desiredsize">максимальный размер</param>
    Public Function CalcMultiplicator(ByRef size As Integer, ByRef desiredsize As Integer) As Integer
        Dim maxSize As Integer = desiredsize
        Dim m As Integer = 0
        Do While (m + 1) * size <= maxSize
            m += 1
        Loop
        Return Math.Max(m, 1)
    End Function

End Class

Public Class ColorSelector

    Private ColorCubeSize As Integer = 255
    Private maxBrightness As Integer = CInt(CDbl(3 * ColorCubeSize) * 0.9)
    Private minBrightness As Integer = CInt(CDbl(3 * ColorCubeSize) * 0.1)

    ''' <summary>Подберет цвета в зависимости от значений в ячейках</summary>
    Public Function MakeColorMap(ByRef grid(,) As Integer) As Color(,)

        Dim imgXSize As Integer = UBound(grid, 1)
        Dim imgYSize As Integer = UBound(grid, 2)
        Dim n As Integer = 0
        For i As Integer = 0 To imgXSize Step 1
            For j As Integer = 0 To imgYSize Step 1
                If n < grid(i, j) Then n = grid(i, j)
            Next j
        Next i
        Dim colorsmap(imgXSize, imgYSize) As Color

        Dim palette(n) As Color
        Dim xSize, ySize, zSize As Integer
        Dim p As Integer = 0
        Do While p < n
            p = 0
            If xSize * ySize * zSize < n Then xSize += 1
            If xSize * ySize * zSize < n Then ySize += 1
            If xSize * ySize * zSize < n Then zSize += 1
            If Not xSize * ySize * zSize < n Then
                Dim dx_ As Double = CoordinateStep(ColorCubeSize, xSize)
                Dim dy_ As Double = CoordinateStep(ColorCubeSize, ySize)
                Dim dz_ As Double = CoordinateStep(ColorCubeSize, zSize)
                For k As Integer = 0 To zSize - 1 Step 1
                    Dim z As Integer = ColorCoordinate(k, dz_, zSize, ColorCubeSize)
                    For j As Integer = 0 To ySize - 1 Step 1
                        Dim y As Integer = ColorCoordinate(j, dy_, ySize, ColorCubeSize)
                        Dim s As Integer = y + z
                        For i As Integer = 0 To xSize - 1 Step 1
                            Dim x As Integer = ColorCoordinate(i, dx_, xSize, ColorCubeSize)
                            If UseColor(s + x) Then p += 1
                        Next i
                    Next j
                Next k
                If p < n Then
                    Dim m As Integer = Math.Min(xSize, Math.Min(ySize, zSize))
                    If m = xSize Then
                        xSize += 1
                    ElseIf m = ySize Then
                        ySize += 1
                    Else
                        zSize += 1
                    End If
                End If
            End If
        Loop

        Dim col(3 * ColorCubeSize) As Collections.Generic.List(Of Color)
        Dim u As Integer = UBound(col)
        For i As Integer = 0 To u Step 1
            col(i) = New Collections.Generic.List(Of Color)
        Next i
        Dim dx As Double = CoordinateStep(ColorCubeSize, xSize)
        Dim dy As Double = CoordinateStep(ColorCubeSize, ySize)
        Dim dz As Double = CoordinateStep(ColorCubeSize, zSize)
        For k As Integer = 0 To zSize - 1 Step 1
            Dim z As Integer = ColorCoordinate(k, dz, zSize, ColorCubeSize)
            For j As Integer = 0 To ySize - 1 Step 1
                Dim y As Integer = ColorCoordinate(j, dy, ySize, ColorCubeSize)
                Dim s As Integer = y + z
                For i As Integer = 0 To xSize - 1 Step 1
                    Dim x As Integer = ColorCoordinate(i, dx, xSize, ColorCubeSize)
                    If UseColor(s + x) Then col(s + x).Add(Color.FromArgb(x, y, z))
                Next i
            Next j
        Next k

        Dim column, row As Integer
        column = 0 : row = u
        For i As Integer = 1 To n Step 1
            Dim c As Boolean = True
            Do While c
                If col(row).Count = column Then
                    row -= 1
                    column = 0
                Else
                    palette(i) = col(row).Item(column)
                    column += 1
                    c = False
                End If
            Loop
        Next i

        Dim tmesh(,) As Integer = CType(grid.Clone, Integer(,))
        Parallel. _
         For(0, imgXSize + 1, _
         Sub(x1 As Integer)
             Dim id As Integer
             For y1 As Integer = 0 To imgYSize Step 1
                 id = tmesh(x1, y1)
                 If id = 0 Then
                     colorsmap(x1, y1) = Color.Black
                 Else
                     colorsmap(x1, y1) = palette(id)
                 End If
             Next y1
         End Sub)

        Return colorsmap
    End Function
    Private Function CoordinateStep(ByRef A As Integer, ByRef ItemsCount As Integer) As Double
        If ItemsCount > 1 Then
            Return CDbl(A) / CDbl(ItemsCount - 1)
        Else
            Return CDbl(A)
        End If
    End Function
    Private Function ColorCoordinate(ByRef n As Integer, ByRef dn As Double, _
                                     ByRef ItemsCount As Integer, ByRef A As Integer) As Integer
        If ItemsCount > 1 Then
            If n < ItemsCount Then
                Return CInt(CDbl(n) * dn)
            Else
                Return A
            End If
        Else
            Return CInt(0.5 * dn)
        End If
    End Function
    Private Function UseColor(ByRef RGBsum As Integer) As Boolean
        If RGBsum <= maxBrightness AndAlso RGBsum >= minBrightness Then
            Return True
        Else
            Return False
        End If
    End Function

End Class

Public Class TerminationCondition
    Friend maxTime As Long
    Dim startTime As Long
    Public ExitFromLoops As Boolean

    ''' <param name="maxT">время на работу</param>
    Public Sub New(ByVal maxT As Integer)
        Call init(CLng(maxT))
    End Sub
    ''' <param name="maxT">время на работу</param>
    Public Sub New(ByVal maxT As Long)
        Call init(maxT)
    End Sub
    Private Sub init(ByRef maxT As Long)
        maxTime = maxT
        startTime = CLng(Environment.TickCount)
        ExitFromLoops = False
    End Sub

    ''' <summary>Установит ExitFromLoops=True, если время истекло</summary>
    Public Sub CheckTime()
        Dim T As Long = CLng(Environment.TickCount)
        Dim d As Long = DeltaTime(startTime, T)
        If d > maxTime Then ExitFromLoops = True
    End Sub

    Private Function DeltaTime(ByRef startT As Long, ByRef endT As Long) As Long
        If endT >= startT Then
            Return endT - startT
        Else
            Return CLng(Integer.MaxValue) - startT + endT - CLng(Integer.MinValue)
        End If
    End Function

End Class

Public Class AttendedObject
    ''' <summary>Номер записи в массиве со всеми посещаемыми объектами</summary>
    Public ReadOnly TypeID As DefMapObjects.Types
    ''' <summary>Длина стороны объекта</summary>
    Public ReadOnly Size As Integer
    ''' <summary>Половина длины стороны объекта</summary>
    Public ReadOnly SizeHalf As Integer
    '''' <summary>Название объекта</summary>
    'Public ReadOnly Name As String
    ''' <summary>Нужно ли размещать охраняющий отряд</summary>
    Public ReadOnly hasExternalGuard As Boolean
    ''' <summary>Площадь, которую нужно выделить под объект</summary>
    Friend ReadOnly Area As Integer
    ''' <summary>Положение объекта по X и Y относительно положения области, выделенной под него</summary>
    Friend ReadOnly dxy As Integer
    ''' <summary>То же, что и Area</summary>
    Friend ReadOnly sArea As Short

    'ByVal objName As String,
    Public Sub New(ByVal objSize As Integer, ByVal objTypeID As DefMapObjects.Types, _
                   Optional ByVal objHasExternalGuard As Boolean = False)
        'Name = objName
        Size = objSize
        TypeID = objTypeID
        hasExternalGuard = objHasExternalGuard

        dxy = 1
        If hasExternalGuard Then dxy += 1
        Area = CInt((Size + 2 * dxy) ^ 2)
        sArea = CShort(Area)
        SizeHalf = CInt((Size - 1) / 2)
    End Sub
End Class

Public Class shortMapFormat

    Public Const ApplyStrictTypesFilter As Boolean = True

    ''' <summary>Состояние тайлов</summary>
    Public landscape(-1, -1) As TileState
    ''' <summary>Объекты местности</summary>
    Public landmarks(-1) As simpleObject
    ''' <summary>Горы</summary>
    Public mountains(-1) As simpleObject
    ''' <summary>Столицы</summary>
    Public capitals(-1) As CapitalObject
    ''' <summary>Шахты</summary>
    Public mines(-1) As simpleObject
    ''' <summary>Торговцы предметами</summary>
    Public merchantsItems(-1) As MerchantItemObject
    ''' <summary>Торговцы заклинаниями</summary>
    Public merchantsSpells(-1) As MerchantSpellObject
    ''' <summary>Наемники</summary>
    Public merchantsUnits(-1) As MerchantUnitObject
    ''' <summary>Тренеры</summary>
    Public trainers(-1) As TrainerObject
    ''' <summary>Отряды</summary>
    Public stacks(-1) As StackObject
    ''' <summary>Руины</summary>
    Public ruins(-1) As RuinObject
    ''' <summary>Города</summary>
    Public cities(-1) As CityObject

    Public Class OwnerType
        ''' <summary>Обозначение расы владельца, используемое генератором</summary>
        Public byGenereator As String
        ''' <summary>Обозначение расы владельца, используемое игрой</summary>
        Public byGame As String

        ''' <param name="genR">Обозначение расы в генераторе</param>
        ''' <param name="d">Инициализированный класс</param>
        Public Sub New(ByRef genR As String, ByRef d As GenDefaultValues)
            byGenereator = genR
            byGame = d.generatorRaceToGameRace(genR.ToUpper)
        End Sub
    End Class

    Public Class simpleObject
        ''' <summary>Положение верхнего левого угла объекта</summary>
        Public pos As Point
        ''' <summary>ID объекта GXXXWWXXXX</summary>
        Public id As String
        ''' <summary>Размер объекта</summary>
        Public size As Size
    End Class
    Public MustInherit Class ObjectWithName
        Inherits simpleObject
        ''' <summary>Название объекта</summary>
        Public objectName As String
    End Class
    Public MustInherit Class ObjectWithDescription
        Inherits ObjectWithName
        ''' <summary>Описание объекта</summary>
        Public objectDescription As String
    End Class
    Public MustInherit Class ObjectWithInternalStack
        Inherits ObjectWithName
        ''' <summary>Настройки генерации внутреннего отряда</summary>
        Public internalStackSettings As AllDataStructues.DesiredStats
        ''' <summary>Внутренний стек</summary>
        Public internalStack As AllDataStructues.Stack
    End Class

    Public Structure TileState
        ''' <summary>Тип местности</summary>
        Public ground As GroundType
        ''' <summary>Раса владельца</summary>
        Public owner As String
        ''' <summary>ID дерева, если оно есть</summary>
        Public treeID As Integer

        ''' <summary>Тип местности</summary>
        Public Enum GroundType
            Plain = 0
            Forest = 1
            'Mountain = 4
            Water = 3
            Road = 5
        End Enum
    End Structure
    Public Class RuinObject
        Inherits ObjectWithInternalStack
        ''' <summary>Ресурсы</summary>
        Public resourcesReward As AllDataStructues.Cost

        ''' <summary>Вернет ID предмета-награды, если таковой имеется. Если нет - Nothing</summary>
        Public Function ItemReward() As String
            If IsNothing(internalStack.items) OrElse internalStack.items.Count = 0 Then
                Return Nothing
            Else
                Return internalStack.items.Item(0)
            End If
        End Function
    End Class
    Public Class CityObject
        Inherits ObjectWithInternalStack
        ''' <summary>Настройки генерации внешнего отряда</summary>
        Public externalStackSettings As AllDataStructues.DesiredStats
        ''' <summary>Внешний стек</summary>
        Public exteternalStack As AllDataStructues.Stack
        ''' <summary>Уровень города</summary>
        Public level As Integer
        ''' <summary>Обозначение расы владельца</summary>
        Public owner As OwnerType
    End Class
    Public Class MerchantItemObject
        Inherits ObjectWithDescription
        ''' <summary>Список предметов</summary>
        Public content() As AllDataStructues.Item
    End Class
    Public Class MerchantSpellObject
        Inherits ObjectWithDescription
        ''' <summary>Список заклинаний</summary>
        Public content() As AllDataStructues.Spell
    End Class
    Public Class MerchantUnitObject
        Inherits ObjectWithDescription
        ''' <summary>Список юнитов</summary>
        Public content() As AllDataStructues.Unit
    End Class
    Public Class TrainerObject
        Inherits ObjectWithDescription
    End Class
    Public Class StackObject
        ''' <summary>Положение отряда</summary>
        Public pos As Point
        ''' <summary>Настройки генерации отряда</summary>
        Public stackSettings As AllDataStructues.DesiredStats
        ''' <summary>Отряд</summary>
        Public stack As AllDataStructues.Stack
        ''' <summary>Обозначение расы владельца</summary>
        Public owner As OwnerType
    End Class
    Public Class CapitalObject
        Inherits ObjectWithName
        ''' <summary>Обозначение расы владельца</summary>
        Public owner As OwnerType
        ''' <summary>ID лорда владельца</summary>
        Public lord As String
        ''' <summary>Имя лорда владельца</summary>
        Public lordName As String
        ''' <summary>Начальный запас ресурсов</summary>
        Public startResources As AllDataStructues.Cost
        ''' <summary>Предметы в столице</summary>
        Public items() As AllDataStructues.Item
    End Class

    'Public Shared Sub checkgroups(ByRef m As Map)
    '    Dim groups As New List(Of Integer)
    '    For y As Integer = 0 To UBound(m.board, 2) Step 1
    '        For x As Integer = 0 To UBound(m.board, 1) Step 1
    '            If m.board(x, y).mapObject.objectID > DefMapObjects.Types.None Or m.board(x, y).stack.GuardLoc Or m.board(x, y).stack.isObjectGuard Or m.board(x, y).stack.PassGuardLoc Then
    '                Dim group As Integer = m.board(x, y).groupID
    '                If groups.Contains(group) Then
    '                    MsgBox("")
    '                End If
    '                groups.Add(group)
    '            End If
    '        Next x
    '    Next y
    'End Sub
    'Public Shared Sub checkraces(ByRef m As Map)
    '    For y As Integer = 0 To UBound(m.board, 2) Step 1
    '        For x As Integer = 0 To UBound(m.board, 1) Step 1
    '            Call checkraces(m, x, y)
    '        Next x
    '    Next y
    'End Sub
    'Public Shared Sub checkraces(ByRef m As Map, x As Integer, y As Integer)
    '
    '    If m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
    '        Dim pos As New Point(x, y)
    '        Dim owner As String = m.board(x, y).mapObject.City.race
    '        Dim level As Integer = m.board(x, y).mapObject.City.level
    '        Dim isGround As Boolean = Not RaceGen.MayBeWater(m, x, y)
    '
    '        Dim desiredStatsExter As AllDataStructues.DesiredStats = Nothing
    '        Dim desiredStatsInter As AllDataStructues.DesiredStats = Nothing
    '        Dim stackExter As AllDataStructues.Stack = Nothing
    '        Dim stackInter As AllDataStructues.Stack = Nothing
    '        If m.groupStats.ContainsKey(m.board(x, y).groupID) Then
    '            desiredStatsExter = m.groupStats.Item(m.board(x, y).groupID)
    '            If desiredStatsExter.Race.Count = 1 AndAlso desiredStatsExter.Race.Item(0) = RaceGen.WaterRace Then
    '                MsgBox("")
    '            End If
    '        End If
    '        If m.groupStats.ContainsKey(-m.board(x, y).groupID) Then
    '            desiredStatsInter = m.groupStats.Item(-m.board(x, y).groupID)
    '            If desiredStatsInter.Race.Count = 1 AndAlso desiredStatsInter.Race.Item(0) = RaceGen.WaterRace Then
    '                MsgBox("")
    '            End If
    '        End If
    '    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins Then
    '    ElseIf m.board(x, y).stack.GuardLoc Or m.board(x, y).stack.PassGuardLoc Or m.board(x, y).stack.isObjectGuard Then
    '        Dim desiredStats As AllDataStructues.DesiredStats = m.groupStats.Item(m.board(x, y).groupID)
    '        Dim isGround As Boolean = Not RaceGen.MayBeWater(m, x, y)
    '        If desiredStats.Race.Count = 1 AndAlso desiredStats.Race.Item(0) = RaceGen.WaterRace And isGround Then
    '            MsgBox("")
    '        End If
    '    End If
    'End Sub
    ''' <summary>Конвертирует карту для более удобной записи в файл</summary>
    ''' <param name="m">Карта</param>
    ''' <param name="settGen">Настройки генерации карты</param>
    ''' <param name="objContent">Полностью инициализированный класс</param>
    ''' <param name="fullSymmetry">Если карта симметрична, сделать отряды и награды абсолютно симметричными (пока не используется)</param>
    ''' <param name="usePlayableRaceUnitsInNeutralStacks">Если True, то в нейтральных отрядах будут использованы юниты из веток развития</param>
    ''' <param name="treesAmont">Количество деревьев для земли каждой расы. ID рас в Races.txt, последний столбец</param>
    Public Shared Function MapConversion(ByRef m As Map, _
                                         ByRef settGen As ImpenetrableMeshGen.GenSettings, _
                                         ByRef ObjectsSize() As ImpenetrableObjects.GlobalMapDecoration, _
                                         ByRef objContent As ObjectsContentSet, _
                                         ByRef fullSymmetry As Boolean, _
                                         ByRef usePlayableRaceUnitsInNeutralStacks As Boolean, _
                                         ByRef treesAmont() As Integer, _
                                         ByRef lang As GenDefaultValues.TextLanguage) As shortMapFormat

        Dim attObjects() As AttendedObject = (New ImpenetrableMeshGen(m.comm)).ActiveObjects
        Dim sName As New SetName(lang, m.comm.defValues.selectedMod)
        Call sName.ResetNames(True, -1)

        Dim allSpells(objContent.spells.Count + objContent.excludedSpells.Count - 1) As AllDataStructues.Spell
        Dim ns As Integer = -1
        For Each spellsList As Dictionary(Of String, AllDataStructues.Spell) In {objContent.spells, objContent.excludedSpells}
            For Each spell As AllDataStructues.Spell In spellsList.Values
                ns += 1
                allSpells(ns) = spell
            Next spell
        Next spellsList

        Dim raceMana As Dictionary(Of Integer, AllDataStructues.Cost()) = ImpenetrableObjects.RacesManaUsing(objContent.randStack.comm, allSpells)
        Dim raceT1mana As New Dictionary(Of Integer, AllDataStructues.Cost)
        For Each id As Integer In raceMana.Keys
            Dim a() As Integer = AllDataStructues.Cost.ToArray(raceMana.Item(id)(1))
            Dim max As Integer = a.Max
            For k As Integer = 0 To UBound(a) Step 1
                If a(k) < max Then a(k) = 0
            Next k
            For k As Integer = 0 To UBound(a) Step 1
                If a(k) = max Then a(k) = 1
            Next k
            raceT1mana.Add(id, AllDataStructues.Cost.ToCost(a))
        Next id

        Dim gLocSettings() As Map.SettingsLoc = copySettings(m, settGen)

        Dim startItems() As AllDataStructues.Item = GenStartItems(objContent)

        Dim res As New shortMapFormat
        ReDim res.landscape(UBound(m.board, 1), UBound(m.board, 2))

        Dim name As String
        Dim allMines, playableRaces As New List(Of String)

        Dim RuinIGen As New AllDataStructues.LootGenSettings(False) With {.JewelItems = New AllDataStructues.ItemGenSettings With {.exclude = True}}

        Dim lordsList() As String
        If usePlayableRaceUnitsInNeutralStacks Then
            lordsList = Nothing
        Else
            ReDim lordsList(settGen.common_settMap.nRaces - 1)
            Dim raceToLord As New Dictionary(Of Integer, String)
            For Each key As String In objContent.randStack.comm.LordsRace.Keys
                Dim v As Integer = objContent.randStack.comm.LordsRace.Item(key)
                If Not raceToLord.ContainsKey(v) Then raceToLord.Add(v, key)
            Next key
            For i As Integer = 0 To settGen.common_settMap.nRaces - 1 Step 1
                lordsList(i) = raceToLord.Item(m.Loc(i).Race)
            Next i
        End If
        objContent.randStack.mapData.mapLords = lordsList

        For y As Integer = 0 To UBound(m.board, 2) Step 1
            For x As Integer = 0 To UBound(m.board, 1) Step 1
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mine Then
                    name = m.board(x, y).mapObject.objectName
                    Call AddObject(res.mines, x, y, objContent.SetMineType(name), attObjects(DefMapObjects.Types.Mine).Size)
                    If Not allMines.Contains(res.mines(UBound(res.mines)).id) Then allMines.Add(res.mines(UBound(res.mines)).id)
                End If
            Next x
        Next y

        Dim tiles(m.board.Length - 1) As Point
        Dim tilesList As New List(Of Integer)
        Dim n As Integer = -1
        For y As Integer = 0 To UBound(m.board, 2) Step 1
            For x As Integer = 0 To UBound(m.board, 1) Step 1
                n += 1
                tilesList.Add(n)
                tiles(n) = New Point(x, y)
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                    playableRaces.Add(objContent.randStack.comm.defValues.capitalToGeneratorRace(m.board(x, y).mapObject.objectName))
                End If
            Next x
        Next y
        For i As Integer = 0 To n Step 1
            Dim pID As Integer = objContent.randStack.comm.RandomSelection(tilesList, True)
            tilesList.Remove(pID)
            Dim x As Integer = tiles(pID).X
            Dim y As Integer = tiles(pID).Y

            'ландшафт
            res.landscape(x, y).owner = objContent.randStack.comm.defValues.RaceNumberToRaceChar(objContent.randStack.comm.RaceIdentifierToSubrace("Neutral"))
            res.landscape(x, y).treeID = -1
            If m.board(x, y).surface.isWater Then
                res.landscape(x, y).ground = TileState.GroundType.Water
            Else
                If Not playableRaces.Contains(objContent.randStack.comm.defValues.RaceNumberToRaceChar(m.board(x, y).mapObject.objRace.Item(0))) Then
                    res.landscape(x, y).owner = objContent.randStack.comm.defValues.RaceNumberToRaceChar(m.board(x, y).mapObject.objRace.Item(0))
                End If
                If m.board(x, y).surface.isForest Then
                    res.landscape(x, y).ground = TileState.GroundType.Forest
                    Dim max As Integer = treesAmont(objContent.randStack.comm.RaceIdentifierToSubrace(res.landscape(x, y).owner)) - 1
                    res.landscape(x, y).treeID = objContent.randStack.rndgen.RndInt(0, max, True)
                ElseIf m.board(x, y).surface.isRoad Then
                    res.landscape(x, y).ground = TileState.GroundType.Road
                Else
                    res.landscape(x, y).ground = TileState.GroundType.Plain
                End If
            End If

            name = m.board(x, y).mapObject.objectName

            'объекты местности
            If Not name = "" Then
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                    Dim subrace As Integer = objContent.randStack.comm.RaceIdentifierToSubrace(objContent.randStack.comm.defValues.capitalToGeneratorRace(name))
                    Dim resources As AllDataStructues.Cost = raceT1mana.Item(subrace) * CDbl(settGen.common_settMap.StartMana / AllDataStructues.Cost.Sum(raceT1mana.Item(subrace)))
                    resources.Gold += settGen.common_settMap.StartGold
                    Call AddObject(res.capitals, x, y, name, objContent, attObjects, sName, resources, startItems)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                    Dim pos As New Point(x, y)
                    Dim owner As String = m.board(x, y).mapObject.City.race
                    Dim level As Integer = m.board(x, y).mapObject.City.level
                    Dim landRace As Integer = m.board(x, y).mapObject.objRace.Item(0)

                    Dim desiredStatsExter As AllDataStructues.DesiredStats = Nothing
                    Dim desiredStatsInter As AllDataStructues.DesiredStats = Nothing
                    Dim stackExter As AllDataStructues.Stack = Nothing
                    Dim stackInter As AllDataStructues.Stack = Nothing
                    If m.groupStats.ContainsKey(m.board(x, y).groupID) Then
                        desiredStatsExter = m.groupStats.Item(m.board(x, y).groupID)
                        Dim gs As New AllDataStructues.CommonStackCreationSettings _
                            With {.StackStats = desiredStatsExter, _
                                  .deltaLeadership = 0, _
                                  .GroundTile = True, _
                                  .NoLeader = False, _
                                  .pos = pos}
                        stackExter = objContent.randStack.Gen(gs)
                    End If
                    If m.groupStats.ContainsKey(-m.board(x, y).groupID) Then
                        desiredStatsInter = m.groupStats.Item(-m.board(x, y).groupID)
                        Dim gs As New AllDataStructues.CommonStackCreationSettings _
                            With {.StackStats = desiredStatsInter, _
                                  .deltaLeadership = 0, _
                                  .GroundTile = True, _
                                  .NoLeader = True, _
                                  .pos = pos}
                        stackInter = objContent.randStack.Gen(gs)
                    End If
                    Call AddObject(res.cities, x, y, name, desiredStatsExter, desiredStatsInter, stackExter, stackInter, owner, level, objContent, attObjects, landRace, sName)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Vendor Then
                    Dim content As List(Of String) = objContent.GetMerchantListSettings(m, gLocSettings, x, y)
                    Dim items As List(Of String) = objContent.MakeMerchantItemsList(New AllDataStructues.DesiredStats With {.shopContent = content}, Nothing, m.log)
                    Call AddObject(res.merchantsItems, x, y, name, items, objContent, attObjects, sName)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mercenary Then
                    Dim content As List(Of String) = objContent.GetMercenariesListSettings(m, gLocSettings, x, y, settGen.common_settMap.nRaces)
                    Dim items As List(Of String) = objContent.MakeMercenariesList(New AllDataStructues.DesiredStats With {.shopContent = content}, m.log)
                    Call AddObject(res.merchantsUnits, x, y, name, items, objContent, attObjects, sName)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mage Then
                    Dim content As List(Of String) = objContent.GetSpellsListSettings(m, gLocSettings, x, y, settGen.common_settMap.nRaces)
                    Dim items As List(Of String) = objContent.MakeSpellsList(New AllDataStructues.DesiredStats With {.shopContent = content}, allMines, m.log)
                    Call AddObject(res.merchantsSpells, x, y, name, items, objContent, attObjects, sName)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Trainer Then
                    Call AddObject(res.trainers, x, y, name, attObjects, sName)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins Then
                    Dim pos As New Point(x, y)
                    Dim desiredStats As AllDataStructues.DesiredStats = m.groupStats.Item(m.board(x, y).groupID)
                    Dim gs As New AllDataStructues.CommonStackCreationSettings _
                     With {.StackStats = desiredStats, _
                           .deltaLeadership = 0, _
                           .GroundTile = Not m.board(x, y).surface.isWater, _
                           .NoLeader = True, _
                           .pos = pos}
                    Dim stack As AllDataStructues.Stack = objContent.randStack.Gen(gs)
                    Dim itemCost As Integer = objContent.randStack.rndgen.RndInt(CInt(0.25 * desiredStats.LootCost), desiredStats.LootCost, True)

                    Dim gi As New AllDataStructues.CommonLootCreationSettings _
                        With {.GoldCost = itemCost, _
                              .IGen = RuinIGen, _
                              .TypeCostRestriction = Nothing, _
                              .pos = pos}
                    Dim loot As String = objContent.randStack.ThingGen(gi)
                    Dim resources As AllDataStructues.Cost = New AllDataStructues.Cost With {.Gold = desiredStats.LootCost}
                    If Not loot = "" Then resources.Gold -= AllDataStructues.Cost.Sum(objContent.randStack.LootCost(loot))
                    stack.items.Clear()
                    stack.items.Add(loot)
                    Call AddObject(res.ruins, x, y, name, desiredStats, stack, resources, attObjects, sName)
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mine Then
                    'do nothing
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.None Then
                    If name.ToUpper.StartsWith(GenDefaultValues.wObjKeyMountain.ToUpper) Then
                        Call AddObject(res.mountains, x, y, name, ImpenetrableObjects.GlobalMapDecoration.MountainSize(name))
                    Else
                        Call AddObject(res.landmarks, x, y, name, ImpenetrableObjects.GlobalMapDecoration.GetObjectSize(ObjectsSize, name))
                    End If
                Else
                    Throw New Exception("shortMapFormat.MapConversion: unknown object type")
                End If
            ElseIf m.board(x, y).stack.GuardLoc Or m.board(x, y).stack.PassGuardLoc Or m.board(x, y).stack.ObjectGuard Then
                Dim desiredStats As AllDataStructues.DesiredStats = m.groupStats.Item(m.board(x, y).groupID)
                Dim isGround As Boolean = Not RaceGen.MayBeWater(m, x, y)
                Dim gs As New AllDataStructues.CommonStackCreationSettings _
                    With {.StackStats = desiredStats, _
                          .deltaLeadership = 0, _
                          .GroundTile = isGround, _
                          .NoLeader = False, _
                          .pos = New Point(x, y)}
                Dim stack As AllDataStructues.Stack = objContent.randStack.Gen(gs)
                If isGround Then
                    Dim leader As AllDataStructues.Unit = objContent.randStack.FindUnitStats(stack.pos(stack.leaderPos))
                    If leader.waterOnly Then
                        Dim txt As String = "Water only leader on ground! Pos: " & x & " " & y
                        Console.WriteLine(txt)
                        Call m.log.Add(txt)
                    End If
                End If
                Call AddObject(res.stacks, x, y, desiredStats, stack, objContent, sName)
            End If
        Next i

        For y As Integer = 0 To UBound(m.board, 2) Step 1
            For x As Integer = 0 To UBound(m.board, 1) Step 1
                name = m.board(x, y).mapObject.objectName
                If Not IsNothing(name) AndAlso Not name = "" Then
                    Dim owner As String = ""
                    Dim size As Integer
                    If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                        owner = objContent.randStack.comm.defValues.capitalToGeneratorRace(name)
                        size = attObjects(m.board(x, y).mapObject.objectID).Size
                    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                        owner = m.board(x, y).mapObject.City.race
                        size = attObjects(m.board(x, y).mapObject.objectID).Size
                    ElseIf name.ToUpper.StartsWith(GenDefaultValues.wObjKeyMountain.ToUpper) Then
                        owner = objContent.randStack.comm.defValues.RaceNumberToRaceChar(objContent.randStack.comm.RaceIdentifierToSubrace("Neutral"))
                        size = ImpenetrableObjects.GlobalMapDecoration.MountainSize(name).Width
                    End If
                    If Not owner = "" Then
                        For x1 As Integer = 0 To size - 1 Step 1
                            For y1 As Integer = 0 To size - 1 Step 1
                                res.landscape(x + x1, y + y1).owner = owner
                            Next y1
                        Next x1
                    End If
                End If
            Next x
        Next y

        Dim stackArray(UBound(res.stacks)) As AllDataStructues.Stack
        For i As Integer = 0 To UBound(res.stacks) Step 1
            stackArray(i) = res.stacks(i).stack
        Next i
        Call sName.GenName(stackArray, objContent.randStack, False)
        For i As Integer = 0 To UBound(res.stacks) Step 1
            res.stacks(i).stack = stackArray(i)
        Next i
        m.log.Add(sName.log.PrintAll)
        m.log.Add("Total stacks count: " & stackArray.Length & vbNewLine)
        Return res
    End Function
    Private Shared Sub AddObject(ByRef AddTo() As simpleObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef size As Size)
        ReDim Preserve AddTo(AddTo.Length)
        AddTo(UBound(AddTo)) = New simpleObject With {.pos = New Point(x, y), _
                                                      .id = name, _
                                                      .size = New Size(size.Width, size.Height)}
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As simpleObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef size As Integer)
        Call AddObject(AddTo, x, y, name, New Size(size, size))
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As StackObject, ByRef x As Integer, ByRef y As Integer, _
                                 ByRef settings As AllDataStructues.DesiredStats, ByRef stack As AllDataStructues.Stack, _
                                 ByRef objContent As ObjectsContentSet, ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim d As GenDefaultValues = objContent.randStack.comm.defValues
        'Call sName.GenName(stack, objContent.randStack, False)
        AddTo(UBound(AddTo)) = New StackObject With {.pos = New Point(x, y), _
                                                     .stack = stack, _
                                                     .stackSettings = settings, _
                                                     .owner = New OwnerType(d.RaceNumberToRaceChar.Item(d.linked_Races.Item("N")), d)}
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As RuinObject, ByRef x As Integer, ByRef y As Integer, _
                                 ByRef name As String, ByRef settings As AllDataStructues.DesiredStats, _
                                 ByRef stack As AllDataStructues.Stack, ByRef rewardRes As AllDataStructues.Cost, _
                                 ByRef attObj() As AttendedObject, _
                                 ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim s As Integer = attObj(DefMapObjects.Types.Ruins).Size
        AddTo(UBound(AddTo)) = New RuinObject With {.pos = New Point(x, y), _
                                                    .id = name, _
                                                    .internalStack = stack, _
                                                    .internalStackSettings = settings, _
                                                    .resourcesReward = rewardRes, _
                                                    .size = New Size(s, s), _
                                                    .objectName = sName.ObjectName(name)}
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As CityObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef settingsExter As AllDataStructues.DesiredStats, ByRef settingsInter As AllDataStructues.DesiredStats, _
                                 ByRef stackExter As AllDataStructues.Stack, ByRef stackInter As AllDataStructues.Stack, _
                                 ByRef owner As String, ByRef level As Integer, ByRef objContent As ObjectsContentSet, ByRef attObj() As AttendedObject, _
                                 ByRef landRace As Integer, ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim d As GenDefaultValues = objContent.randStack.comm.defValues
        Dim s As Integer = attObj(DefMapObjects.Types.City).Size
        AddTo(UBound(AddTo)) = New CityObject With {.pos = New Point(x, y), _
                                                    .id = name, _
                                                    .exteternalStack = stackExter, _
                                                    .internalStack = stackInter, _
                                                    .externalStackSettings = settingsExter, _
                                                    .internalStackSettings = settingsInter, _
                                                    .level = level, _
                                                    .owner = New OwnerType(owner, d), _
                                                    .size = New Size(s, s), _
                                                    .objectName = sName.CityName(name, landRace)}
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As MerchantItemObject, ByRef x As Integer, ByRef y As Integer, _
                                 ByRef name As String, ByRef content As List(Of String), _
                                 ByRef objContent As ObjectsContentSet, ByRef attObj() As AttendedObject, _
                                 ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim s As Integer = attObj(DefMapObjects.Types.Vendor).Size
        AddTo(UBound(AddTo)) = New MerchantItemObject With {.pos = New Point(x, y), _
                                                            .id = name, _
                                                            .size = New Size(s, s), _
                                                            .objectName = sName.ObjectName(name), _
                                                            .objectDescription = sName.ObjectDescription(name)}
        ReDim AddTo(UBound(AddTo)).content(content.Count - 1)
        Dim n As Integer = -1
        For Each item As String In content
            n += 1
            AddTo(UBound(AddTo)).content(n) = objContent.randStack.FindItemStats(item)
        Next item
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As MerchantSpellObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef content As List(Of String), ByRef objContent As ObjectsContentSet, ByRef attObj() As AttendedObject, _
                                 ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim s As Integer = attObj(DefMapObjects.Types.Mage).Size
        AddTo(UBound(AddTo)) = New MerchantSpellObject With {.pos = New Point(x, y), _
                                                             .id = name, _
                                                             .size = New Size(s, s), _
                                                             .objectName = sName.ObjectName(name), _
                                                             .objectDescription = sName.ObjectDescription(name)}
        ReDim AddTo(UBound(AddTo)).content(content.Count - 1)
        Dim n As Integer = -1
        For Each item As String In content
            n += 1
            AddTo(UBound(AddTo)).content(n) = objContent.randStack.FindSpellStats(item)
        Next item
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As MerchantUnitObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef content As List(Of String), ByRef objContent As ObjectsContentSet, ByRef attObj() As AttendedObject, _
                                 ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim s As Integer = attObj(DefMapObjects.Types.Mercenary).Size
        AddTo(UBound(AddTo)) = New MerchantUnitObject With {.pos = New Point(x, y), _
                                                            .id = name, _
                                                            .size = New Size(s, s), _
                                                            .objectName = sName.ObjectName(name), _
                                                            .objectDescription = sName.ObjectDescription(name)}
        ReDim AddTo(UBound(AddTo)).content(content.Count - 1)
        Dim n As Integer = -1
        For Each item As String In content
            n += 1
            AddTo(UBound(AddTo)).content(n) = objContent.randStack.FindUnitStats(item)
        Next item
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As TrainerObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef attObj() As AttendedObject, _
                                 ByRef sName As SetName)
        ReDim Preserve AddTo(AddTo.Length)
        Dim s As Integer = attObj(DefMapObjects.Types.Trainer).Size
        AddTo(UBound(AddTo)) = New TrainerObject With {.pos = New Point(x, y), _
                                                       .id = name, _
                                                       .size = New Size(s, s), _
                                                       .objectName = sName.ObjectName(name), _
                                                       .objectDescription = sName.ObjectDescription(name)}
    End Sub
    Private Shared Sub AddObject(ByRef AddTo() As CapitalObject, ByRef x As Integer, ByRef y As Integer, ByRef name As String, _
                                 ByRef objContent As ObjectsContentSet, ByRef attObj() As AttendedObject, _
                                 ByRef sName As SetName, ByRef startResources As AllDataStructues.Cost, ByRef startItems() As AllDataStructues.Item)
        ReDim Preserve AddTo(AddTo.Length)
        Dim r As String = objContent.randStack.comm.defValues.capitalToGeneratorRace(name)
        Dim subrace As Integer = objContent.randStack.comm.RaceIdentifierToSubrace(r)
        Dim s As Integer = attObj(DefMapObjects.Types.Capital).Size
        Dim lordName As String = sName.LordName(subrace, False)
        AddTo(UBound(AddTo)) = New CapitalObject With {.pos = New Point(x, y), _
                                                       .id = name, _
                                                       .owner = New OwnerType(r, objContent.randStack.comm.defValues), _
                                                       .lord = objContent.LordRandomizer(r, False), _
                                                       .lordName = lordName, _
                                                       .size = New Size(s, s), _
                                                       .objectName = sName.CityName(name, subrace), _
                                                       .startResources = AllDataStructues.Cost.Copy(startResources), _
                                                       .items = startItems}
    End Sub

    Private Shared Function copySettings(ByRef m As Map, ByRef settGen As ImpenetrableMeshGen.GenSettings) As Map.SettingsLoc()
        If settGen.genMode = ImpenetrableMeshGen.GenSettings.genModes.simple Then
            Return Map.SettingsLoc.ToArray(settGen.simple_settRaceLoc, _
                                           settGen.simple_settCommLoc, _
                                           settGen.common_settMap.nRaces, _
                                           m.Loc.Length)
        Else
            Return Map.SettingsLoc.Copy(settGen.template_settLoc)
        End If
    End Function

    Private Shared Function GenStartItems(ByRef objContent As ObjectsContentSet) As AllDataStructues.Item()
        Dim startItems(-1) As AllDataStructues.Item
        For Each item As AllDataStructues.Item In objContent.randStack.AllItems
            If Not objContent.randStack.comm.IsExcluded(item) AndAlso item.type = GenDefaultValues.ItemTypes.ressurection_elixir Then
                For i As Integer = 0 To 2 Step 1
                    ReDim Preserve startItems(startItems.Length)
                    startItems(UBound(startItems)) = AllDataStructues.Item.Copy(item)
                Next i
                Exit For
            End If
        Next item
        Dim healPotCost As Integer = 1300
        Dim forceExit As Boolean = True
        Dim maxCost As Integer
        Dim minCost As Integer = Integer.MaxValue
        Dim s As Integer
        For Each item As AllDataStructues.Item In objContent.randStack.AllItems
            If Not objContent.randStack.comm.IsExcluded(item) _
            AndAlso item.type = GenDefaultValues.ItemTypes.healing_elixir Then
                s = AllDataStructues.Cost.Sum(item.itemCost)
                maxCost = Math.Max(maxCost, s)
                minCost = Math.Min(minCost, s)
            End If
        Next item
        Do While healPotCost > 0
            For Each item As AllDataStructues.Item In objContent.randStack.AllItems
                s = AllDataStructues.Cost.Sum(item.itemCost)
                If Not objContent.randStack.comm.IsExcluded(item) _
                AndAlso item.type = GenDefaultValues.ItemTypes.healing_elixir _
                AndAlso s > 0 Then
                    If s < maxCost OrElse minCost = maxCost Then
                        forceExit = False
                        If objContent.randStack.rndgen.Rand(0, 1, True) < 0.1 * Math.Min(1, healPotCost / s) Then
                            ReDim Preserve startItems(startItems.Length)
                            startItems(UBound(startItems)) = AllDataStructues.Item.Copy(item)
                            healPotCost -= s
                            If healPotCost <= 0 Then Exit Do
                        End If
                    End If
                End If
            Next item
            If forceExit Then Exit Do
        Loop
        Return startItems
    End Function
End Class

Public Class Map
    ''' <summary>Список локаций. Первые в списке - стартовые по числу играбельных рас на карте</summary>
    Public Loc() As Location
    ''' <summary>Поле карты, содержащее свойства каждой клетки</summary>
    Public board(,) As Cell
    ''' <summary>Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</summary>
    Public ReadOnly xSize As Integer
    ''' <summary>Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</summary>
    Public ReadOnly ySize As Integer
    ''' <summary>Идентификатор симметрии, применяемой при генерации</summary>
    Public ReadOnly symmID As Integer
    ''' <summary>Желаемые статы для каждой группы стэков. Индексы групп (key) хранятся в board(,).groupID. В этом словаре могут присутствовать и отрицательные ключи - для внутренней охраны городов. Положительный ключ - внешняя охрана, отрицательный, но равный по модулю - внутренняя охрана того же города, при этом внутренняя охрана не всегда может присутствовать</summary>
    Public groupStats As Dictionary(Of Integer, AllDataStructues.DesiredStats)
    ''' <summary>Какие этапы закончены</summary>
    Public complited As ComplitedSteps

    ''' <summary>Запись о ходе генерации карты</summary>
    Public log As Log

    Public comm As Common

    ''' <param name="xDim">Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</param>
    ''' <param name="yDim">Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</param>
    ''' <param name="SymmApplied">Идентификатор симметрии, применяемой при генерации</param>
    ''' <param name="common_data">Пекредаем инициализированный класс только если нужно что-то писать в лог</param>
    Public Sub New(ByVal xDim As Integer, ByVal yDim As Integer, ByVal SymmApplied As Integer, Optional ByRef common_data As Common = Nothing)
        xSize = xDim
        ySize = yDim
        symmID = SymmApplied
        ReDim board(xSize, ySize)
        For x As Integer = 0 To xSize Step 1
            For y As Integer = 0 To ySize Step 1
                board(x, y).ClearLocIDArray() 'New List(Of Integer)
                board(x, y).mapObject.NewTagsList()
            Next y
        Next x
        If Not IsNothing(common_data) Then
            log = New Log(common_data)
            log.Enable()
            comm = common_data
        End If
    End Sub

    Public Structure Cell
        ''' <summary>ID локаций, с которыми связана эта клетка</summary>
        Dim locID() As Integer 'List(Of Integer)
        ''' <summary>Для объектов с одинаковым ID выставляются одинаковые параметры генерации отрядов и лута или одинаковый класс.
        ''' При необходимости выставляются одинаковые отряды и лут.</summary>
        Dim groupID As Integer

        Dim surface As AreaData
        Dim passability As CrossabilityData
        Dim mapObject As ObjectData
        Dim stack As TroopData

        Public Structure TroopData
            ''' <summary>True, отряд является внешней или внутренней охраной посещаемого объекта</summary>
            Dim ObjectGuard As Boolean
            ''' <summary>True, если на клетке нужно разместить обычный отряд</summary>
            Dim GuardLoc As Boolean
            ''' <summary>True, если на клетке нужно разместить отряд, охраняющий проход в соседнюю локацию</summary>
            Dim PassGuardLoc As Boolean
            ''' <summary>Расы, допустимые для воинов отряда, занимающего эту клетку</summary>
            Dim stackRace As List(Of Integer)
        End Structure
        Public Structure ObjectData
            ''' <summary>Если клетка является углом посещаемого объекта c наименьшей координатой по X и Y, то здесь хранится ID объекта</summary>
            Dim objectID As DefMapObjects.Types
            ''' <summary>Если клетка является углом объекта c наименьшей координатой по X и Y, то здесь хранится ID объекта, как он записан в ресурсах игры</summary>
            Dim objectName As String
            ''' <summary>Расы, допустимые для объекта, занимающего эту клетку</summary>
            Dim objRace As List(Of Integer)
            ''' <summary>Если 0, то город будет нейтральным, иначе - такая же раса, как и раса-владелец указанной локации</summary>
            Dim City As SettingsLoc.SettingsRaceCity
            ''' <summary>Объекты с какими тэгами можно расположить в данном тайле</summary>
            Private ObjectTags As List(Of String)

            Public Function TagsList() As List(Of String)
                Return ObjectTags
            End Function
            Public Function InTagsList(ByRef v As String) As Boolean
                Return ObjectTags.Contains(v.ToUpper)
            End Function
            Public Sub NewTagsList()
                ObjectTags = New List(Of String)
            End Sub
            Public Sub AddTag(ByRef v As String)
                ObjectTags.Add(v.ToUpper)
            End Sub
            Public Sub RemoveTag(ByRef v As String)
                ObjectTags.Remove(v.ToUpper)
            End Sub
        End Structure
        Public Structure CrossabilityData
            ''' <summary>True, если на клетке должен стоять непроходимый непосещаемый объект</summary>
            Dim isBorder As Boolean
            ''' <summary>True, если клетка обязательно должна быть проходимой</summary>
            Dim isPenetrable As Boolean
            ''' <summary>True, если клетка является частью прохода между локациями</summary>
            Dim isPass As Boolean
            ''' <summary>True, если клетка находится под посещаемым объектом</summary>
            Dim isAttended As Boolean
        End Structure
        Public Structure AreaData
            ''' <summary>True, если на клетке вода</summary>
            Dim isWater As Boolean
            ''' <summary>True, если на клетке дорога</summary>
            Dim isRoad As Boolean
            ''' <summary>True, если на клетке лес</summary>
            Dim isForest As Boolean
        End Structure

        Public Sub AddToLocIDArray(ByRef value As Integer)
            If locID.Count = 0 Then
                ReDim locID(0)
            Else
                ReDim Preserve locID(locID.Length)
            End If
            locID(UBound(locID)) = value
        End Sub
        Public Sub ClearLocIDArray()
            ReDim locID(-1)
        End Sub
    End Structure

    Public Structure ComplitedSteps
        Dim LoationsCreation_Done As Boolean
        Dim MeshTestI_Done As Boolean
        Dim StacksPlacing_Done As Boolean
        Dim MeshTestII_Done As Boolean
        Dim StacksDesiredStatsGen_Done As Boolean
        Dim WaterCreation_Done As Boolean
        Dim StacksRaceGen_Done As Boolean
        Dim ImpenetrableObjectsPlacing_Done As Boolean
        Dim PenetrableObjectsPlacing_Done As Boolean
    End Structure

    Public Class SettingsLoc
        ''' <summary>Средний радиус локаций</summary>
        Public AverageRadius As Integer
        ''' <summary>Локации будут в форме эллипсов со случайным эксцентриситетом от (1-D)/(1+D) до (1+D)/(1-D)</summary>
        Public maxEccentricityDispersion As Double
        ''' <summary>При достаточном количестве места будут создаваться локации размером от (1-D)*R до (1+D)*R.
        ''' Когда свободного места станет недостаточно R начнет постепенно уменьшаться до половины от начального значения</summary>
        Public maxRadiusDispersion As Double

        ''' <summary>Плотность непроходимых объектов внутри локации (0 - нет, 1 - максимальное количество)</summary>
        Public DecorationsAmount As Double

        '''<summary>Количество золотых шахт на локацию</summary>
        Public maxGoldMines As Double
        '''<summary>Количество источников маны на локацию</summary>
        Public maxManaSources As Double
        '''<summary>Количество нейтральных городов на локацию</summary>
        Public maxCities As Double
        '''<summary>Количество торговцев на локацию</summary>
        Public maxVendors As Double
        '''<summary>Количество лагерей наемников на локацию</summary>
        Public maxMercenaries As Double
        '''<summary>Количество башен мага на локацию</summary>
        Public maxMages As Double
        '''<summary>Количество тренеров на локацию</summary>
        Public maxTrainers As Double
        '''<summary>Количество руин на локацию</summary>
        Public maxRuins As Double

        '''<summary>Минимальное расстояние между отрядами</summary>
        Public minStackToStackDist As Double

        '''<summary>Примерное количество опыта за убийство всех отрядов в локации</summary>
        Public expAmount As Double

        '''<summary>Минимальный уровень заклинаний в лавке мага</summary>
        Public mageSpellsMaxLevel As Integer
        '''<summary>Максимальный уровень заклинаний в лавке мага</summary>
        Public mageSpellsMinLevel As Integer
        '''<summary>Количество заклинаний в лавке мага</summary>
        Public mageSpellsCount As Integer
        '''<summary>Могут ли встречаться в лавке мага заклинания, действующие на всю карту</summary>
        Public mageGlobalSpellsEnabled As Boolean

        '''<summary>Максимальная планка опыта у маленьких наемников (для больших в два раза выше)</summary>
        Public mercenariesMaxExpBar As Integer
        '''<summary>Минимальная планка опыта у маленьких наемников (для больших в два раза выше)</summary>
        Public mercenariesMinExpBar As Integer
        '''<summary>Количество наемников в лагере</summary>
        Public mercenariesCount As Integer

        '''<summary>Максимальная стоимость расходного предмета у торговца</summary>
        Public merchMaxConsumableItemCost As Integer
        '''<summary>Минимальная стоимость расходного предмета у торговца</summary>
        Public merchMinConsumableItemCost As Integer
        '''<summary>Максимальная стоимость нерасходного предмета у торговца</summary>
        Public merchMaxNonconsumableItemCost As Integer
        '''<summary>Минимальная стоимость нерасходного предмета у торговца</summary>
        Public merchMinNonconsumableItemCost As Integer
        '''<summary>Полная стоимость лута у торговца</summary>
        Public merchItemsCost As Integer

        '''<summary>Множитель силы отряда в руинах</summary>
        Public ruinsPowerMultiplicator As Double
        '''<summary>Множитель ценности лута в руинах</summary>
        Public ruinsWealthMultiplicator As Double
        '''<summary>Множитель силы отрядов в городах</summary>
        Public citiesPowerMultiplicator As Double
        '''<summary>Множитель ценности лута в городах</summary>
        Public citiesWealthMultiplicator As Double

        '''<summary>Города, принадлежащие играбельным расам</summary>
        Public RaceCities() As SettingsRaceCity

        ''' <summary>Масштабировать количество посещаемых объектов и опыт за убийство всех отрядовв в локации</summary>
        Public scaleContent As Boolean

        ''' <summary>Список возможных базовых рас для локации.
        ''' В случае с локациями играбельных рас определяет расы нейтральных отрядов в локации. 
        ''' Список рас можно посмотреть в Resources\Constants.txt, параметр LocRacesBlocks.
        ''' Nothing, если нужен случайный выбор</summary>
        Public possibleRaces() As String

        ''' <summary>True, если локация должна быть соединена с соседними независимо от глобальных настроек</summary>
        Public ConnectWithAllNeighboringLocations As Boolean

        '    Private Checked As Boolean
        '    ''' <summary>Проверит корректность параметров. Вернет пустое сообщение, если все нормально</summary>
        '    Public Function Check() As String
        '
        '        Dim fields As New Dictionary(Of String, String)
        '
        '        fields.Add("AverageRadius", AverageRadius.ToString)
        '        fields.Add("maxEccentricityDispersion", maxEccentricityDispersion.ToString)
        '        fields.Add("maxRadiusDispersion", maxRadiusDispersion.ToString)
        '        fields.Add("maxGoldMines", maxGoldMines.ToString)
        '        fields.Add("maxManaSources", maxManaSources.ToString)
        '        fields.Add("maxCities", maxCities.ToString)
        '        fields.Add("maxVendors", maxVendors.ToString)
        '        fields.Add("maxMercenaries", maxMercenaries.ToString)
        '        fields.Add("maxMages", maxMages.ToString)
        '        fields.Add("maxTrainers", maxTrainers.ToString)
        '        fields.Add("maxRuins", maxRuins.ToString)
        '        fields.Add("minStackToStackDist", minStackToStackDist.ToString)
        '        fields.Add("expAmount", expAmount.ToString)
        '        fields.Add("mageSpellsMaxLevel", mageSpellsMaxLevel.ToString)
        '        fields.Add("mageSpellsMinLevel", mageSpellsMinLevel.ToString)
        '        fields.Add("mageSpellsCount", mageSpellsCount.ToString)
        '        'fields.Add("mageGlobalSpellsEnabled", mageGlobalSpellsEnabled.ToString)
        '        fields.Add("mercenariesMaxExpBar", mercenariesMaxExpBar.ToString)
        '        fields.Add("mercenariesMinExpBar", mercenariesMinExpBar.ToString)
        '        fields.Add("mercenariesCount", mercenariesCount.ToString)
        '        fields.Add("merchMaxConsumableItemCost", merchMaxConsumableItemCost.ToString)
        '        fields.Add("merchMinConsumableItemCost", merchMinConsumableItemCost.ToString)
        '        fields.Add("merchMaxNonconsumableItemCost", merchMaxNonconsumableItemCost.ToString)
        '        fields.Add("merchMinNonconsumableItemCost", merchMinNonconsumableItemCost.ToString)
        '        fields.Add("merchItemsCost", merchItemsCost.ToString)
        '
        '        Dim msg As String = ""
        '        Dim less0 As New List(Of String)
        '        Dim equal0 As New List(Of String)
        '        Dim less1 As New List(Of String)
        '        Dim equal1 As New List(Of String)
        '        Dim greater1 As New List(Of String)
        '
        '        For Each k As String In fields.Keys
        '            less0.Add(k)
        '        Next k
        '        equal0.AddRange({"AverageRadius", "expAmount"})
        '        If maxMages > 0 Then
        '            If mageSpellsMaxLevel < mageSpellsMinLevel Then msg &= vbNewLine & "mageSpellsMaxLevel < mageSpellsMinLevel"
        '            equal0.AddRange({"mageSpellsMaxLevel", "mageSpellsMinLevel", "mageSpellsCount"})
        '        End If
        '        If maxMercenaries > 0 Then
        '            If mercenariesMaxExpBar < mercenariesMinExpBar Then msg &= vbNewLine & "mercenariesMaxExpBar < mercenariesMinExpBar"
        '            equal0.AddRange({"mercenariesMaxExpBar", "mercenariesMinExpBar", "mercenariesCount"})
        '        End If
        '        If maxVendors > 0 Then
        '            If merchMaxConsumableItemCost < merchMinConsumableItemCost Then msg &= vbNewLine & "merchMaxConsumableItemCost < merchMinConsumableItemCost"
        '            If merchMaxNonconsumableItemCost < merchMinNonconsumableItemCost Then msg &= vbNewLine & "merchMaxNonconsumableItemCost < merchMinNonconsumableItemCost"
        '            equal0.AddRange({"merchMaxConsumableItemCost", "merchMinConsumableItemCost", "merchMaxNonconsumableItemCost", "merchMinNonconsumableItemCost", "merchItemsCost"})
        '        End If
        '        less1.AddRange({"minStackToStackDist"})
        '        equal1.AddRange({"maxEccentricityDispersion", "maxRadiusDispersion"})
        '        greater1.AddRange({"maxEccentricityDispersion", "maxRadiusDispersion"})
        '
        '        msg &= Map.CheckGenParameters(fields, less0, equal0, less1, equal1, greater1)
        '        If msg.Length > 0 Then msg = msg.Substring(1)
        '        Checked = True
        '        Return msg
        '    End Function
        '    ''' <summary>True, если проверка параметров запускалась</summary>
        '    Public Function isChecked() As Boolean
        '        Return Checked
        '    End Function

        Public Structure ExtendedBlockData
            Public data As Dictionary(Of String, String)
            Public label As String
        End Structure

        ''' <summary>Сменить владельцев в RaceCities, если номер локации расы-владельца больше 0.
        ''' Если полученный номер станет больше nRaces, то уменьшится на nRaces</summary>
        ''' <param name="i">Добавить к номеру владельца это число</param>
        ''' <param name="nRaces">Количество рас</param>
        Public Sub IncrementOwner(ByRef i As Integer, ByRef nRaces As Integer)
            If IsNothing(RaceCities) Then Exit Sub
            For n As Integer = 0 To UBound(RaceCities) Step 1
                Call RaceCities(n).IncrementOwner(i, nRaces)
            Next n
        End Sub

        ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас</param>
        ''' <param name="settCommLoc">Настройки для остальных локацийт</param>
        ''' <param name="nRaces">Количество играбельных рас</param>
        ''' <param name="nLocs">Количество локаций</param>
        Public Shared Function ToArray(ByRef settRaceLoc As SettingsLoc, ByRef settCommLoc As SettingsLoc, ByVal nRaces As Integer, ByRef nLocs As Integer) As SettingsLoc()
            Dim res(nLocs - 1) As SettingsLoc
            For i As Integer = 0 To nRaces - 1 Step 1
                res(i) = SettingsLoc.Copy(settRaceLoc)
            Next i
            For i As Integer = nRaces To nLocs - 1 Step 1
                res(i) = SettingsLoc.Copy(settCommLoc)
            Next i
            Return res
        End Function

        Public Shared Function Copy(ByRef v As SettingsLoc) As SettingsLoc
            If IsNothing(v) Then Return Nothing
            Dim r() As String = Nothing
            If Not IsNothing(v.possibleRaces) Then r = CType(v.possibleRaces.Clone, String())
            Return New SettingsLoc With { _
            .AverageRadius = v.AverageRadius, _
            .maxEccentricityDispersion = v.maxEccentricityDispersion, _
            .maxRadiusDispersion = v.maxRadiusDispersion, _
            .DecorationsAmount = v.DecorationsAmount, _
            .maxGoldMines = v.maxGoldMines, _
            .maxManaSources = v.maxManaSources, _
            .maxCities = v.maxCities, _
            .maxVendors = v.maxVendors, _
            .maxMercenaries = v.maxMercenaries, _
            .maxMages = v.maxMages, _
            .maxTrainers = v.maxTrainers, _
            .maxRuins = v.maxRuins, _
            .minStackToStackDist = v.minStackToStackDist, _
            .expAmount = v.expAmount, _
            .mageSpellsMaxLevel = v.mageSpellsMaxLevel, _
            .mageSpellsMinLevel = v.mageSpellsMinLevel, _
            .mageSpellsCount = v.mageSpellsCount, _
            .mageGlobalSpellsEnabled = v.mageGlobalSpellsEnabled, _
            .mercenariesMaxExpBar = v.mercenariesMaxExpBar, _
            .mercenariesMinExpBar = v.mercenariesMinExpBar, _
            .mercenariesCount = v.mercenariesCount, _
            .merchMaxConsumableItemCost = v.merchMaxConsumableItemCost, _
            .merchMinConsumableItemCost = v.merchMinConsumableItemCost, _
            .merchMaxNonConsumableItemCost = v.merchMaxNonconsumableItemCost, _
            .merchMinNonConsumableItemCost = v.merchMinNonconsumableItemCost, _
            .merchItemsCost = v.merchItemsCost, _
            .scaleContent = v.scaleContent, _
            .possibleRaces = r, _
            .RaceCities = SettingsRaceCity.Copy(v.RaceCities), _
            .ConnectWithAllNeighboringLocations = v.ConnectWithAllNeighboringLocations, _
            .ruinsPowerMultiplicator = v.ruinsPowerMultiplicator, _
            .ruinsWealthMultiplicator = v.ruinsWealthMultiplicator, _
            .citiesPowerMultiplicator = v.citiesPowerMultiplicator, _
            .citiesWealthMultiplicator = v.citiesWealthMultiplicator}
        End Function
        Public Shared Function Copy(ByRef v() As SettingsLoc) As SettingsLoc()
            If IsNothing(v) Then Return Nothing
            Dim res(UBound(v)) As SettingsLoc
            For i As Integer = 0 To UBound(v) Step 1
                res(i) = SettingsLoc.Copy(v(i))
            Next i
            Return res
        End Function

        Public Shared Function Read(ByRef path As String, ByRef dataColumn As Integer) As SettingsLoc()
            Dim data() As ExtendedBlockData = ReadRawData(path, dataColumn)
            Dim res(UBound(data)) As SettingsLoc
            For i As Integer = 0 To UBound(data) Step 1
                res(i) = New SettingsLoc
                Call res(i).Read(data(i).data)
            Next i
            Return res
        End Function
        Private Sub Read(ByRef txt() As String, ByRef blockNumber As Integer, ByRef dataColumn As Integer, _
                         ByRef nextLoop As Boolean, ByRef baseFilePath As String)
            Dim data As Dictionary(Of String, String) = ReadBlock(txt, GenDefaultValues.wTemplate_LocationKeyword, _
                                                                  blockNumber, dataColumn, False, baseFilePath, True)
            Call Read(data)
        End Sub
        Private Sub Read(ByRef data As Dictionary(Of String, String))
            Dim fields() As String = ClassFieldsHandler.GetFieldsNamesList(Me, {"RaceCities"})
            For Each f As String In fields
                Call Map.ReadValue(ClassFieldsHandler.GetField(Me, f), data, GenDefaultValues.wTemplate_LocationKeyword, CChar(";"))
            Next f
        End Sub
        Public Shared Function ReadRawData(ByRef baseFilePath As String, ByRef dataColumn As Integer, Optional ByRef txt() As String = Nothing) As ExtendedBlockData()
            If IsNothing(txt) Then txt = ValueConverter.TxtSplit(IO.File.ReadAllText(baseFilePath))
            Dim blockNumber As Integer = 0
            Dim res(-1) As ExtendedBlockData
            Dim nextLoop As Boolean = True
            Do While nextLoop
                blockNumber += 1
                ReDim Preserve res(blockNumber - 1)
                res(blockNumber - 1) = New ExtendedBlockData
                res(blockNumber - 1).data = ReadBlock(txt, GenDefaultValues.wTemplate_LocationKeyword, blockNumber, dataColumn, _
                                                      False, baseFilePath, True, res(blockNumber - 1).label)
                If IsNothing(res(blockNumber - 1).data) Then nextLoop = False
            Loop
            If UBound(res) > -1 Then ReDim Preserve res(UBound(res) - 1)
            Return res
        End Function

        Public Structure SettingsRaceCity
            ''' <summary>Уровень города</summary>
            Dim level As Integer
            ''' <summary>Номер локации расых-владельца. Если 0 - нейтрал</summary>
            Dim owner As Integer
            ''' <summary>Идентификатор расы-владельца</summary>
            Dim race As String

            Public Shared Function Copy(ByRef v As SettingsRaceCity) As SettingsRaceCity
                Return New SettingsRaceCity With {.level = v.level, .owner = v.owner, .race = v.race}
            End Function
            Public Shared Function Copy(ByRef v() As SettingsRaceCity) As SettingsRaceCity()
                If Not IsNothing(v) Then
                    Dim rc(UBound(v)) As SettingsRaceCity
                    For i As Integer = 0 To UBound(v) Step 1
                        rc(i) = SettingsRaceCity.Copy(v(i))
                    Next i
                    Return rc
                Else
                    Return Nothing
                End If
            End Function

            ''' <summary>Сменить владельца, если номер локации расых-владельца больше 0.
            ''' Если полученный номер станет больше nRaces, то уменьшится на nRaces</summary>
            ''' <param name="i">Добавить к номеру владельца это число</param>
            ''' <param name="nRaces">Количество рас</param>
            Public Sub IncrementOwner(ByRef i As Integer, ByRef nRaces As Integer)
                If owner = 0 Then Exit Sub
                If owner < 0 Then Throw New Exception("Unexpected city owner ID: " & owner)
                owner += i
                Do While owner > nRaces
                    owner -= nRaces
                Loop
            End Sub

            Public Shared Function Print(ByRef v As SettingsRaceCity) As String
                Return v.level & "#" & v.owner
            End Function
            Public Shared Function Read(ByRef v As String) As SettingsRaceCity
                Dim s() As String = v.Split(CChar("#"))
                Return New SettingsRaceCity With {.level = ValueConverter.StrToInt(s(0), v, "RaceCities"), _
                                                  .owner = ValueConverter.StrToInt(s(1), v, "RaceCities"), _
                                                  .race = ""}
            End Function

        End Structure

        Public Shared Function Print(ByRef v As SettingsLoc) As String
            Dim res As String = ""
            Dim fields() As String = ClassFieldsHandler.GetFieldsNamesList(v, Nothing)
            For Each f As String In fields
                res &= vbNewLine & ImpenetrableMeshGen.GenSettings.Print(ClassFieldsHandler.GetField(v, f), CChar(";"))
            Next f
            Return res.Substring(1)
        End Function

    End Class
    Public Class SettingsMap
        ''' <summary>Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 24)</summary>
        Public xSize As Integer
        ''' <summary>Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 48)</summary>
        Public ySize As Integer
        ''' <summary>Минимальное расстояние между проходами</summary>
        Public minPassDist As Double
        ''' <summary>Минимальная ширина проходов</summary>
        Public minPassWidth As Double
        ''' <summary>Количество рас</summary>
        Public nRaces As Integer
        ''' <summary>Генератор будет располагать локации со столицами так, чтобы для каждой из локаций выполнялось следующиее условие:
        ''' R1*(1+T) >= R2, при этом R2 > R1, где R1 и R2 - расстояние до двух ближайших локаций со столицами</summary>
        Public RaceLocsDistTolerance As Double
        ''' <summary>Расставлять ли стражей проходов между локациями</summary>
        Public AddGuardsBetweenLocations As Boolean
        ''' <summary>Множитель силы стражей проходов между локациями</summary>
        Public PassGuardsPowerMultiplicator As Double
        ''' <summary>Множитель силы стражей посещаемых объектов</summary>
        Public ObjectGuardsPowerMultiplicator As Double
        ''' <summary>Отношение максимального опыта, получаемого за зачистку локации среднего размера, к минимальному.
        ''' Чем дальше локация от ближайшей столицы и чем ближе к центру, тем больше опыта за ее зачистку</summary>
        Public LocExpRatio As Double
        ''' <summary>Множитель стоимости лута нейтралов</summary>
        Public Wealth As Double
        ''' <summary>Количество воды на карте. 0 - без воды, 1 - очень много</summary>
        Public WaterAmount As Double
        '''<summary>Максимальный уровень заклинаний в столице</summary>
        Public SpellsMaxLevel As Integer
        ''' <summary>Количество дорог на карте. 0 - без дорог, 1 - максимальное количество</summary>
        Public RoadsAmount As Double
        ''' <summary>Количество леса на карте. 0 - без леса, 1 - максимальное количество</summary>
        Public ForestAmount As Double
        ''' <summary>Размещение столиц. 0 - ближе к центру карты, 1 - ближе к краю карты</summary>
        Public CapitalRepulsion As Double

        ''' <summary>Определяет свободу перемещения по карте. 0 - генератор не старается соединять локации, 1 - генератор соединит каждую локацию с каждой соседней (но расовые старается не соединять)</summary>
        Public PassageCreationChance As Double

        ''' <summary>Применять ли операции симметрии</summary>
        Public ApplySymmetry As Boolean
        '''<summary> -1 - случайная симметрия, если .ApplySymmetry=True, игнорируется, если .ApplySymmetry=False.
        ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
        ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M.
        ''' Смотри SymmetryOperations </summary>
        Public SymmetryClass As Integer

        ''' <summary>Расы игроков. Nothing, если нужны случайные</summary>
        Public PlayersRaces() As String
        '''<summary>Начальное количество золота</summary>
        Public StartGold As Integer
        '''<summary>Начальное количество родной маны</summary>
        Public StartMana As Integer

        Private Checked As Boolean
        ''' <summary>Проверит корректность параметров. Вернет пустое сообщение, если все нормально</summary>
        Public Function Check() As String

            Dim fields As New Dictionary(Of String, String)

            fields.Add("xSize", xSize.ToString)
            fields.Add("ySize", ySize.ToString)
            fields.Add("minPassDist", minPassDist.ToString)
            fields.Add("minPassWidth", minPassWidth.ToString)
            fields.Add("nRaces", nRaces.ToString)
            fields.Add("RaceLocsDistTolerance", RaceLocsDistTolerance.ToString)
            'fields.Add("AddGuardsBetweenLocations", AddGuardsBetweenLocations.ToString)
            fields.Add("PassGuardsPowerMultiplicator", PassGuardsPowerMultiplicator.ToString)
            fields.Add("ObjectGuardsPowerMultiplicator", ObjectGuardsPowerMultiplicator.ToString)
            fields.Add("LocExpRatio", LocExpRatio.ToString)
            fields.Add("Wealth", Wealth.ToString)
            fields.Add("WaterAmount", WaterAmount.ToString)
            fields.Add("SpellsMaxLevel", SpellsMaxLevel.ToString)
            fields.Add("RoadsAmount", RoadsAmount.ToString)
            fields.Add("ForestAmount", ForestAmount.ToString)
            fields.Add("CapitalRepulsion", CapitalRepulsion.ToString)
            fields.Add("PassageCreationChance", PassageCreationChance.ToString)

            Dim msg As String = ""
            Dim less0 As New List(Of String)
            Dim equal0 As New List(Of String)
            Dim less1 As New List(Of String)
            Dim equal1 As New List(Of String)
            Dim greater1 As New List(Of String)

            For Each k As String In fields.Keys
                less0.Add(k)
            Next k
            equal0.AddRange({"SpellsMaxLevel", "ObjectGuardsPowerMultiplicator"})
            If AddGuardsBetweenLocations Then equal0.AddRange({"PassGuardsPowerMultiplicator"})
            less1.AddRange({"xSize", "ySize", "minPassDist", "minPassWidth", "nRaces", "LocExpRatio"})
            equal1.AddRange({"nRaces"})
            greater1.AddRange({"WaterAmount", "RoadsAmount", "ForestAmount", "PassageCreationChance"})

            msg &= Map.CheckGenParameters(fields, less0, equal0, less1, equal1, greater1)
            If msg.Length > 0 Then msg = msg.Substring(1)
            Checked = True
            Return msg
        End Function
        ''' <summary>True, если проверка параметров запускалась</summary>
        Public Function isChecked() As Boolean
            Return Checked
        End Function

        Public Shared Function Read(ByRef path As String) As SettingsMap
            Dim res As New SettingsMap
            Call res.Read(Nothing, path)
            Return res
        End Function
        Private Sub Read(ByRef txt() As String, ByRef baseFilePath As String)
            Dim data As Dictionary(Of String, String) = ReadRawData(baseFilePath, txt)

            Dim fields() As String = ClassFieldsHandler.GetFieldsNamesList(Me, {"Checked"})
            For Each f As String In fields
                Call Map.ReadValue(ClassFieldsHandler.GetField(Me, f), data, GenDefaultValues.wTemplate_MapKeyword, CChar(";"))
            Next f
        End Sub
        Public Shared Function ReadRawData(ByRef baseFilePath As String, Optional ByRef txt() As String = Nothing) As Dictionary(Of String, String)
            If IsNothing(txt) Then txt = ValueConverter.TxtSplit(IO.File.ReadAllText(baseFilePath))
            Return ReadBlock(txt, GenDefaultValues.wTemplate_MapKeyword, 1, 1, True, baseFilePath, True)
        End Function

        Public Shared Function Copy(ByRef v As SettingsMap) As SettingsMap
            If IsNothing(v) Then Return Nothing
            Dim r() As String = Nothing
            If Not IsNothing(v.PlayersRaces) Then r = CType(v.PlayersRaces.Clone, String())
            Return New SettingsMap With {.xSize = v.xSize, _
                                         .ySize = v.ySize, _
                                         .minPassDist = v.minPassDist, _
                                         .minPassWidth = v.minPassWidth, _
                                         .nRaces = v.nRaces, _
                                         .RaceLocsDistTolerance = v.RaceLocsDistTolerance, _
                                         .AddGuardsBetweenLocations = v.AddGuardsBetweenLocations, _
                                         .PassGuardsPowerMultiplicator = v.PassGuardsPowerMultiplicator, _
                                         .ObjectGuardsPowerMultiplicator = v.ObjectGuardsPowerMultiplicator, _
                                         .LocExpRatio = v.LocExpRatio, _
                                         .Wealth = v.Wealth, _
                                         .WaterAmount = v.WaterAmount, _
                                         .SpellsMaxLevel = v.SpellsMaxLevel, _
                                         .RoadsAmount = v.RoadsAmount, _
                                         .ForestAmount = v.ForestAmount, _
                                         .CapitalRepulsion = v.CapitalRepulsion, _
                                         .PassageCreationChance = v.PassageCreationChance, _
                                         .ApplySymmetry = v.ApplySymmetry, _
                                         .SymmetryClass = v.SymmetryClass, _
                                         .PlayersRaces = r, _
                                         .StartGold = v.StartGold, _
                                         .StartMana = v.StartMana, _
                                         .Checked = v.Checked}
        End Function

        Public Shared Function Print(ByRef v As SettingsMap) As String
            Dim res As String = ""
            Dim fields() As String = ClassFieldsHandler.GetFieldsNamesList(v, {"Checked"})
            For Each f As String In fields
                res &= vbNewLine & ImpenetrableMeshGen.GenSettings.Print(ClassFieldsHandler.GetField(v, f), CChar(";"))
            Next f
            Return res.Substring(1)
        End Function

    End Class

#Region "Reader"
    ''' <param name="fileContent">Содержимое файла после ValueConverter.TxtSplit</param>
    ''' <param name="blockType">Тип блока</param>
    ''' <param name="blockNumber">Номер блока > 0</param>
    Protected Friend Shared Function ReadBlock(ByRef fileContent() As String, _
                                               ByRef blockType As String, _
                                               ByRef blockNumber As Integer, _
                                               ByRef readColumn As Integer, _
                                               ByRef throwExceptionIfNoBlock As Boolean, _
                                               ByRef baseFilePath As String, _
                                               ByRef AddReadFromFileIfExists As Boolean, _
                                               Optional ByRef blockLabel As String = "") As Dictionary(Of String, String)
        Dim startLine As Integer = -1
        Dim n As Integer = 0
        For i As Integer = 0 To UBound(fileContent) Step 1
            If fileContent(i).ToUpper.StartsWith((GenDefaultValues.wTemplate_NewBlockKeyword & blockType).ToUpper) Then
                n += 1
                If n = blockNumber Then
                    Dim s() As String = fileContent(i).Split(CChar(" "))
                    If s.Length > 1 Then blockLabel = s(1)
                    startLine = i
                    Exit For
                End If
            End If
        Next i
        If startLine = -1 Then
            If throwExceptionIfNoBlock Then Throw New Exception("Could not find line " & GenDefaultValues.wTemplate_NewBlockKeyword & blockType)
            Return Nothing
        End If

        Dim fields As New Dictionary(Of String, String)
        For i As Integer = startLine + 1 To UBound(fileContent) Step 1
            If fileContent(i).ToUpper.StartsWith(GenDefaultValues.wTemplate_NewBlockKeyword.ToUpper) Then Exit For
            Dim s() As String = fileContent(i).Split(CChar(" "))
            If fileContent(i).ToUpper.StartsWith(GenDefaultValues.wTemplate_ReadFromFileKeyword.ToUpper) Then
                If AddReadFromFileIfExists Then
                    Call AddParameter(fields, s, blockType, blockNumber, readColumn, baseFilePath)
                End If
                Dim fileName As String = s(1)
                Dim searchBlockLabel As String
                If blockType.ToUpper = GenDefaultValues.wTemplate_MapKeyword.ToUpper Then
                    searchBlockLabel = ""
                Else
                    searchBlockLabel = s(2)
                End If
                Call ReadFromOtherFile(baseFilePath, fileName, blockType, searchBlockLabel, readColumn, fields)
            Else
                Call AddParameter(fields, s, blockType, blockNumber, readColumn, baseFilePath)
            End If
        Next i
        Return fields
    End Function
    Private Shared Sub AddParameter(ByRef fields As Dictionary(Of String, String), ByRef s() As String, _
                                    ByRef blockType As String, ByRef blockNumber As Integer, _
                                    ByRef readColumn As Integer, ByRef baseFilePath As String)
        If fields.ContainsKey(s(0).ToUpper) Then fields.Remove(s(0).ToUpper)
        If s.Length > 1 Then
            fields.Add(s(0).ToUpper, s(Math.Min(UBound(s), readColumn)))
        Else
            Throw New Exception("Пустой параметр " & s(0) & " в блоке " & blockType & " #" & blockNumber & ". Файл: " & baseFilePath)
        End If
    End Sub
    Protected Friend Shared Sub ReadFromOtherFile(ByRef baseFile As String, ByRef readFromFile As String, _
                                                  ByRef blockType As String, ByRef searchField As String, _
                                                  ByRef readColumn As Integer, ByRef readTo As Dictionary(Of String, String))
        Dim f As String = IO.Path.GetDirectoryName(baseFile) & "\" & readFromFile
        Dim fileContent() As String = ValueConverter.TxtSplit(IO.File.ReadAllText(f))
        Dim blockN As Integer = 0
        Dim blockName As String = ""
        For i As Integer = 0 To UBound(fileContent) Step 1
            If fileContent(i).ToUpper.StartsWith((GenDefaultValues.wTemplate_NewBlockKeyword & blockType).ToUpper) Then
                Dim s() As String = fileContent(i).Split(CChar(" "))
                If StopBlockSearch(blockType, searchField, s) Then
                    blockName = s(0)
                    Exit For
                End If
            End If
        Next i
        If blockName = "" Then Throw New Exception("Could not find line block with name " & searchField)

        For i As Integer = 0 To UBound(fileContent) Step 1
            If fileContent(i).ToUpper.StartsWith(blockName.ToUpper) Then
                blockN += 1
                Dim s() As String = fileContent(i).Split(CChar(" "))
                If StopBlockSearch(blockType, searchField, s) Then Exit For
            End If
        Next i

        Dim data As Dictionary(Of String, String) = ReadBlock(fileContent, _
            blockName.Replace(GenDefaultValues.wTemplate_NewBlockKeyword, ""), blockN, readColumn, True, f, False)
        For Each k As String In data.Keys
            If readTo.ContainsKey(k.ToUpper) Then readTo.Remove(k.ToUpper)
            readTo.Add(k.ToUpper, data.Item(k))
        Next k
    End Sub
    Private Shared Function StopBlockSearch(ByRef searchBlockType As String, ByRef searchLabel As String, ByRef s() As String) As Boolean
        Dim u As String = searchBlockType.ToUpper
        If u = GenDefaultValues.wTemplate_MapKeyword.ToUpper Then Return True
        If u = GenDefaultValues.wTemplate_CreationKeyword.ToUpper Then Return True
        If u = GenDefaultValues.wTemplate_LocationKeyword.ToUpper _
          AndAlso s.Length > 1 AndAlso s(1).ToUpper = searchLabel.ToUpper Then Return True
        Return False
    End Function
    Protected Friend Shared Function ReadValue(ByRef name As String, ByRef value As String, _
                                               ByRef data As Dictionary(Of String, String), ByRef blockName As String) As String
        If data.ContainsKey(name.ToUpper) Then
            value = data.Item(name.ToUpper)
        Else
            Throw New Exception("Could not find value for " & name & " in " & blockName)
        End If
        Return value
    End Function
    Protected Friend Shared Function ReadValue(ByRef name As String, ByRef value As Double, _
                                               ByRef data As Dictionary(Of String, String), ByRef blockName As String) As Double
        Dim txt As String = ""
        Call ReadValue(name, txt, data, blockName)
        value = ValueConverter.StrToDbl(txt, ImpenetrableMeshGen.GenSettings.Print(name, txt), "")
        Return value
    End Function
    Protected Friend Shared Function ReadValue(ByRef name As String, ByRef value As Integer, _
                                               ByRef data As Dictionary(Of String, String), ByRef blockName As String) As Integer
        Dim txt As String = ""
        Call ReadValue(name, txt, data, blockName)
        value = ValueConverter.StrToInt(txt, ImpenetrableMeshGen.GenSettings.Print(name, txt), "")
        Return value
    End Function
    Protected Friend Shared Function ReadValue(ByRef name As String, ByRef value As Boolean, _
                                               ByRef data As Dictionary(Of String, String), ByRef blockName As String) As Boolean
        Dim txt As String = ""
        Call ReadValue(name, txt, data, blockName)
        value = ValueConverter.StrToBool(txt)
        Return value
    End Function
    Protected Friend Shared Function ReadValue(ByRef name As String, ByRef value() As String, _
                                               ByRef data As Dictionary(Of String, String), ByRef blockName As String, _
                                               ByRef delimiter As Char) As String()
        Dim txt As String = ""
        Call ReadValue(name, txt, data, blockName)
        If txt.ToUpper = "Nothing".ToUpper Then
            value = Nothing
        Else
            value = txt.Split(delimiter)
        End If
        Return value
    End Function
    Protected Friend Shared Function ReadValue(ByRef name As String, ByRef value() As SettingsLoc.SettingsRaceCity, _
                                               ByRef data As Dictionary(Of String, String), ByRef blockName As String, _
                                               ByRef delimiter As Char) As SettingsLoc.SettingsRaceCity()
        Dim txt As String = ""
        Call ReadValue(name, txt, data, blockName)
        If txt.ToUpper = "Nothing".ToUpper Then
            value = Nothing
        Else
            Dim s() As String = txt.Split(delimiter)
            ReDim value(UBound(s))
            For i As Integer = 0 To UBound(s) Step 1
                value(i) = SettingsLoc.SettingsRaceCity.Read(s(i))
            Next i
        End If
        Return value
    End Function
    Protected Friend Shared Sub ReadValue(ByRef valueField As ClassFieldsHandler.GetFieldResult, _
                                          ByRef data As Dictionary(Of String, String), ByRef blockName As String, _
                                          ByRef delimiter As Char, Optional ByVal searchValueByName As String = "")
        Dim name As String
        If searchValueByName = "" Then
            name = valueField.searchResultField.Name
        Else
            name = searchValueByName
        End If
        If valueField.searchResultField.FieldType.FullName = GetType(String).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, _
                                                  ReadValue(name, CType(valueField.searchResult, String), data, blockName))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(String()).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, _
                                                  ReadValue(name, CType(valueField.searchResult, String()), data, blockName, delimiter))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Double).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, _
                                                  ReadValue(name, CType(valueField.searchResult, Double), data, blockName))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Integer).FullName Then
            valueField.searchResultField.SetValue(valueField.LastParent, _
                                                  ReadValue(name, CType(valueField.searchResult, Integer), data, blockName))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(Boolean).FullName Then
            valueField.searchResultField.SetValue(valueField.parents(UBound(valueField.parents)), _
                                                  ReadValue(name, CType(valueField.searchResult, Boolean), data, blockName))
        ElseIf valueField.searchResultField.FieldType.FullName = GetType(SettingsLoc.SettingsRaceCity()).FullName Then
            valueField.searchResultField.SetValue(valueField.parents(UBound(valueField.parents)), _
                                                  ReadValue(name, CType(valueField.searchResult, SettingsLoc.SettingsRaceCity()), data, blockName, delimiter))
        Else
            Throw New Exception("Unexpected variable type")
        End If
    End Sub

    Private Shared Function CheckGenParameters(ByRef fields As Dictionary(Of String, String), ByRef less0 As List(Of String), ByRef equal0 As List(Of String), _
                                               ByRef less1 As List(Of String), ByRef equal1 As List(Of String), ByRef greater1 As List(Of String)) As String
        Dim result As String = ""
        For Each k As String In less0
            If CDbl(fields.Item(k)) < 0 Then result &= vbNewLine & k & " < 0"
        Next k
        For Each k As String In equal0
            If CDbl(fields.Item(k)) = 0 Then result &= vbNewLine & k & " = 0"
        Next k
        For Each k As String In less1
            If CDbl(fields.Item(k)) < 1 Then result &= vbNewLine & k & " < 1"
        Next k
        For Each k As String In equal1
            If CDbl(fields.Item(k)) = 1 Then result &= vbNewLine & k & " = 1"
        Next k
        For Each k As String In greater1
            If CDbl(fields.Item(k)) > 1 Then result &= vbNewLine & k & " > 1"
        Next k
        Return result
    End Function
#End Region

    ''' <summary>Вернет True, если все нормально, иначе стоит перегенерировать</summary>
    Public Function TestMap() As String
        Dim imp As New ImpenetrableMeshGen(comm)
        If Not complited.LoationsCreation_Done Then
            Throw New Exception("Сначала нужно выполнить ImpenetrableMeshGen.SymmGen или ImpenetrableMeshGen.UnsymmGen")
        End If
        If IsNothing(board) Then Return "Occured some error. Change creation parameters"

        For x As Integer = 0 To xSize Step 1
            For y As Integer = 0 To ySize Step 1
                If board(x, y).passability.isBorder And board(x, y).passability.isAttended Then
                    Return "Warning: border and object are on the same place"
                ElseIf board(x, y).passability.isBorder And board(x, y).passability.isPass Then
                    Return "Warning: border and pass are on the same place"
                ElseIf board(x, y).passability.isBorder And board(x, y).passability.isPenetrable Then
                    Return "Warning: border and penetrable cell are on the same place"
                ElseIf board(x, y).passability.isAttended And board(x, y).passability.isPenetrable Then
                    Return "Warning: object and penetrable cell are on the same place"
                ElseIf board(x, y).passability.isAttended And board(x, y).passability.isPass Then
                    Return "Warning: object and pass are on the same place"
                End If
                If complited.StacksPlacing_Done And board(x, y).stack.GuardLoc Then
                    If board(x, y).passability.isBorder Then
                        Return "Warning: border and guard are on the same place"
                    ElseIf board(x, y).passability.isAttended And board(x, y).mapObject.objectID = DefMapObjects.Types.None Then
                        Return "Warning: object and guard are on the same place"
                    ElseIf (board(x, y).mapObject.objectID = DefMapObjects.Types.City Or _
                            board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins) And board(x, y).groupID < 1 Then
                        Return "Warning: group for internal guard for object is zero"
                    ElseIf board(x, y).mapObject.objectID > DefMapObjects.Types.None AndAlso imp.ActiveObjects(board(x, y).mapObject.objectID).hasExternalGuard Then
                        Return "Warning: object with external guard has internal one"
                    End If
                End If
                If complited.StacksPlacing_Done Then
                    If Not board(x, y).stack.GuardLoc And ((board(x, y).mapObject.objectID = DefMapObjects.Types.City And board(x, y).mapObject.City.owner = 0) Or _
                                                     board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins) Then
                        Return "Warning: internal guard for object is not set"
                    End If
                    If board(x, y).stack.GuardLoc And board(x, y).mapObject.objectID = DefMapObjects.Types.City And board(x, y).mapObject.City.owner > 0 Then
                        Return "Warning: internal guard for race city is set"
                    End If
                End If
                If complited.StacksPlacing_Done And board(x, y).stack.PassGuardLoc Then
                    If board(x, y).passability.isBorder Then
                        Return "Warning: border and pass guard are on the same place"
                    ElseIf board(x, y).passability.isAttended Then
                        Return "Warning: object and pass guard are on the same place"
                    ElseIf board(x, y).stack.GuardLoc Then
                        Return "Warning: common guard and pass guard are on the same place"
                    ElseIf board(x, y).groupID < 1 Then
                        Return "Warning: group for pass guard is zero"
                    End If
                End If
            Next y
        Next x
        If complited.StacksPlacing_Done Then
            complited.MeshTestII_Done = True
        Else
            complited.MeshTestI_Done = True
        End If
        Return ""
    End Function

    Public Sub PrintObjectsPositions(ByRef log As Log, ByRef ObjectsSize As Dictionary(Of String, Size))
        If Not log.IsEnabled Then Exit Sub

        Dim imp As New ImpenetrableMeshGen(comm)

        Dim objList() As String = New String() {"None", "Capital", "City", "Vendor", "Mercenary", _
                                                "Mage", "Trainer", "Ruins", "Mine"}
        log.Add("----Objects list----")
        log.Add("Map size:" & vbTab & xSize & vbTab & ySize)
        For i As Integer = 0 To UBound(objList) Step 1
            log.Add("----Type: " & objList(i) & "----")
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If board(x, y).mapObject.objectID = i _
                    And ((i = DefMapObjects.Types.None And Not board(x, y).mapObject.objectName = "") _
                         Or Not i = DefMapObjects.Types.None) Then
                        Dim basic As String = board(x, y).mapObject.objectName & " ; pos: " & vbTab & x & vbTab & y
                        Dim min As String = ""
                        Dim max As String = ""
                        Dim x1, x2, y1, y2 As Integer
                        If i = DefMapObjects.Types.None Then
                            x1 = x
                            y1 = y
                            If Not IsNothing(ObjectsSize) Then
                                x2 = x1 + ObjectsSize.Item(board(x, y).mapObject.objectName).Width - 1
                                y2 = y1 + ObjectsSize.Item(board(x, y).mapObject.objectName).Height - 1
                            Else
                                x2 = x1
                                y2 = y1
                            End If
                        Else
                            x1 = x - imp.ActiveObjects(i).dxy
                            y1 = y - imp.ActiveObjects(i).dxy
                            x2 = x + imp.ActiveObjects(i).dxy + imp.ActiveObjects(i).Size - 1
                            y2 = y + imp.ActiveObjects(i).dxy + imp.ActiveObjects(i).Size - 1
                        End If
                        log.Add(basic & PrintPos(x1, y1, x2, y2))
                    End If
                Next x
            Next y
        Next i
        log.Add("----End of objects list----")
        log.Add("----Stacks positions list----")
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If (board(x, y).stack.GuardLoc Or board(x, y).stack.PassGuardLoc) And board(x, y).mapObject.objectID = DefMapObjects.Types.None Then log.Add(x & vbTab & y)
            Next x
        Next y
        log.Add("----End of stack positions list----")
    End Sub
    Private Function PrintPos(ByRef minX As Integer, ByRef minY As Integer, ByRef maxX As Integer, ByRef maxY As Integer) As String
        Dim res As String = " ; minPos: " & minX
        Dim addWarning As Boolean
        If minX < 0 Then
            res &= "(!)"
            addWarning = True
        End If
        res &= vbTab & minY
        If minY < 0 Then
            res &= "(!)"
            addWarning = True
        End If
        res &= " ; maxPos: " & maxX
        If maxX > xSize Then
            res &= "(!)"
            addWarning = True
        End If
        res &= vbTab & maxY
        If maxY > ySize Then
            res &= "(!)"
            addWarning = True
        End If
        If addWarning Then res &= " !!!!!!!!!!!!!!!!"
        Return res
    End Function

    Public Sub Clear()
        Loc = Nothing
        board = Nothing
        groupStats = Nothing
        complited = Nothing
    End Sub

    Public Function MapConversion(ByRef settGen As ImpenetrableMeshGen.GenSettings,
                                  ByRef ObjectsSize() As ImpenetrableObjects.GlobalMapDecoration, _
                                  ByRef objContent As ObjectsContentSet, _
                                  ByRef fullSymmetry As Boolean, _
                                  ByRef usePlayableRaceUnitsInNeutralStacks As Boolean, _
                                  ByRef treesAmont() As Integer, _
                                  ByRef lang As GenDefaultValues.TextLanguage) As shortMapFormat
        Return shortMapFormat.MapConversion(Me, settGen, ObjectsSize, objContent, fullSymmetry, usePlayableRaceUnitsInNeutralStacks, treesAmont, lang)
    End Function

End Class

Public Class StackLocationsGen

    Private genmap As ImpenetrableMeshGen
    Private symm As SymmetryOperations
    Private comm As Common

    Public Sub New(ByVal c As Common, Optional ByRef gm As ImpenetrableMeshGen = Nothing)
        If IsNothing(gm) Then
            genmap = New ImpenetrableMeshGen(c)
        Else
            genmap = gm
        End If
        symm = genmap.symm
        comm = c
    End Sub

    ''' <summary>Расставляет локации для отрядов на карту с подготовленную в ImpenetrableMeshGen
    ''' Для руин и городов выставляет локации с параметрами отрядов там же, где и хранится objectID.
    ''' Если не получится, вернет False</summary>
    ''' <param name="m">Карта с расставленными объектами и границами между локациями</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону</param>
    ''' <param name="maxGenTime">Максимальное время на операцию расстановки стражей проходов между локациями.
    ''' Она обычно производится меньше чем за секунду, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту</param>
    Public Function Gen(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                        ByVal settRaceLoc As Map.SettingsLoc, _
                        ByVal settCommLoc As Map.SettingsLoc, _
                        ByRef maxGenTime As Integer) As Boolean
        'If Not settRaceLoc.isChecked Then Throw New Exception("Check parameters via settRaceLoc.Check()")
        'If Not settCommLoc.isChecked Then Throw New Exception("Check parameters via settCommLoc.Check()")
        Dim a() As Map.SettingsLoc = Map.SettingsLoc.ToArray(settRaceLoc, settCommLoc, settMap.nRaces, m.Loc.Length)
        Return Gen(m, settMap, a, maxGenTime)
    End Function

    ''' <summary>Расставляет локации для отрядов на карту с подготовленную в ImpenetrableMeshGen
    ''' Для руин и городов выставляет локации с параметрами отрядов там же, где и хранится objectID.
    ''' Если не получится, вернет False</summary>
    ''' <param name="m">Карта с расставленными объектами и границами между локациями</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settLoc">Настройки для каждой локации. Первыми должны идти стартовые локации рас.
    ''' Коментарий к настройкам стартовых локаций играбельных рас:
    ''' дробная часть определяет шанс округления большую сторону.
    ''' Комментарий к настройкам остальных локаций:
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади, заданной в настройках (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт </param>
    ''' <param name="maxGenTime">Максимальное время на операцию расстановки стражей проходов между локациями.
    ''' Она обычно производится меньше чем за секунду, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту</param>
    Public Function Gen(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                        ByVal settLoc() As Map.SettingsLoc, _
                        ByRef maxGenTime As Integer) As Boolean

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        'For i As Integer = 0 To UBound(settLoc) Step 1
        '    If Not settLoc(i).isChecked Then Throw New Exception("Check parameters via settLoc(" & i & ").Check()")
        'Next i

        If Not m.complited.LoationsCreation_Done Or Not m.complited.MeshTestI_Done Then
            Throw New Exception("Сначала нужно выполнить ImpenetrableMeshGen.SymmGen или " & _
                                "ImpenetrableMeshGen.UnsymmGen и протестировать результат с помощью m.TestMap")
        End If

        Call ImpenetrableMeshGen.RecheckPassStatus(m, settMap)

        Dim tmpm As Map = m
        Dim GroupID As Integer
        Dim t0 As Integer = Environment.TickCount

        Call PlaceCommonStacks(tmpm, settMap, settLoc, GroupID)

        If settMap.AddGuardsBetweenLocations Then
            Dim term As New TerminationCondition(maxGenTime)
            Dim guards(UBound(m.Loc))() As Point
            guards = PlasePassesGuards(tmpm, settMap, False, term)
            If term.ExitFromLoops Then
                Call m.log.Add("Stack locations gen: time limit termination")
                Return False
            End If
            Dim gList() As Point = ConvertPointsArray({guards})

            'Call RemoveExcessPassGuards(tmpm, settMap, gList)

            Call PlacePassGuards(tmpm, gList, GroupID, settMap)
        End If
        m = tmpm

        Call m.log.Add("Stacks positions creation: " & Environment.TickCount - t0 & " ms")
        m.complited.StacksPlacing_Done = True
        Return True
    End Function

    Private Sub PlaceCommonStacks(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                                  ByVal settLoc() As Map.SettingsLoc, _
                                  ByRef GroupID As Integer)
        Dim tmpm As Map = m
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mine Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.ActiveObjectsPlacer.ObjectBorders(m.board(x, y).mapObject.objectID, _
                                                                                                      x, y, genmap.ActiveObjects)
                    b.minX = Math.Max(b.minX, 0)
                    b.minY = Math.Max(b.minY, 0)
                    b.maxX = Math.Min(b.maxX, m.xSize)
                    b.maxY = Math.Min(b.maxY, m.ySize)
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If m.board(i, j).passability.isPenetrable And Not m.board(i, j).passability.isPass Then
                                m.board(i, j).passability.isPenetrable = False
                            End If
                        Next i
                    Next j
                End If
                GroupID = Math.Max(GroupID, m.board(x, y).groupID)
            Next x
        Next y
        Dim posList(UBound(m.Loc)) As List(Of Point)
        Parallel.For(0, m.Loc.Length, _
         Sub(i As Integer)
             If Not tmpm.Loc(i).IsObtainedBySymmery Then
                 'If i < settMap.nRaces Then
                 '    posList(i) = FillLocation(tmpm, settMap, settRaceLoc, tmpm.Loc(i).ID)
                 'Else
                 '    posList(i) = FillLocation(tmpm, settMap, settCommLoc, tmpm.Loc(i).ID)
                 'End If
                 posList(i) = FillLocation(tmpm, settMap, settLoc(i), tmpm.Loc(i).ID)
             End If
         End Sub)

        For i As Integer = 0 To UBound(m.Loc) Step 1
            If Not m.Loc(i).IsObtainedBySymmery Then
                For Each p As Point In posList(i)
                    GroupID += 1
                    If m.symmID > -1 Then
                        Dim pp() As Point = symm.ApplySymm(p, settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(pp) Step 1
                            Dim tx As Integer = pp(k).X
                            Dim ty As Integer = pp(k).Y
                            m.board(pp(k).X, pp(k).Y).groupID = GroupID
                            m.board(pp(k).X, pp(k).Y).stack.GuardLoc = True
                        Next k
                    Else
                        m.board(p.X, p.Y).groupID = GroupID
                        m.board(p.X, p.Y).stack.GuardLoc = True
                    End If
                Next p
            End If
        Next i

        Dim protect(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID > DefMapObjects.Types.None AndAlso genmap.ActiveObjects(m.board(x, y).mapObject.objectID).hasExternalGuard Then
                    protect(x + genmap.ActiveObjects(m.board(x, y).mapObject.objectID).Size, _
                            y + genmap.ActiveObjects(m.board(x, y).mapObject.objectID).Size) = True
                End If
            Next x
        Next y

        Dim minStackDist As Integer = 2
        'Dim mDistR As Double = Math.Min(settRaceLoc.minStackToStackDist, minStackDist)
        'Dim mDistC As Double = Math.Min(settCommLoc.minStackToStackDist, minStackDist)
        Dim minDistance(UBound(settLoc)) As Double
        Dim tolerance As Integer = 1 '= CInt(Math.Ceiling(Math.Max(mDistR, mDistC)))
        For i As Integer = 0 To UBound(settLoc) Step 1
            minDistance(i) = Math.Min(settLoc(i).minStackToStackDist, minStackDist)
            tolerance = Math.Max(tolerance, CInt(Math.Ceiling(minDistance(i))))
        Next i

        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).stack.GuardLoc Then
                    Dim d1, d2 As Double
                    d1 = minDistance(m.board(x, y).locID(0) - 1)
                    'If m.board(x, y).locID(0) <= settMap.nRaces Then
                    '    d1 = mDistR
                    'Else
                    '    d1 = mDistC
                    'End If
                    Dim t As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, tolerance)
                    For j As Integer = t.minY To t.maxY Step 1
                        For i As Integer = t.minX To t.maxX Step 1
                            If m.board(i, j).stack.GuardLoc And (Not x = i Or Not y = j) Then
                                Dim d As Double = New Point(x, y).SqDist(i, j)
                                d2 = minDistance(m.board(i, j).locID(0) - 1)
                                'If m.board(i, j).locID(0) <= settMap.nRaces Then
                                '    d2 = mDistR
                                'Else
                                '    d2 = mDistC
                                'End If
                                If d < Math.Min(d1, d2) ^ 2 Then
                                    Dim p() As Point = New Point() {New Point(i, j), New Point(x, y)}
                                    For k As Integer = 0 To 1 Step 1
                                        If Not protect(p(k).X, p(k).Y) Then
                                            If m.symmID > -1 Then
                                                Dim pp() As Point = symm.ApplySymm(p(k), settMap.nRaces, m, 1)
                                                For Each item As Point In pp
                                                    m.board(item.X, item.Y).stack.GuardLoc = False
                                                Next item
                                            Else
                                                m.board(p(k).X, p(k).Y).stack.GuardLoc = False
                                            End If
                                            If k = 1 Then
                                                i = t.maxX
                                                j = t.maxY
                                            End If
                                        End If
                                    Next k
                                End If
                            End If
                        Next i
                    Next j
                End If
            Next x
        Next y

        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                    If tmpm.board(x, y).mapObject.City.owner > 0 Then
                        tmpm.board(x, y).stack.GuardLoc = False
                    Else
                        tmpm.board(x, y).stack.GuardLoc = True
                    End If
                ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins Then
                    m.board(x, y).stack.GuardLoc = True
                End If
            Next x
        Next y
        m = tmpm
    End Sub
    Private Function FillLocation(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                  ByRef settLoc As Map.SettingsLoc, ByRef LocID As Integer) As List(Of Point)

        Dim b As New Location.Borders With {.minX = Integer.MaxValue, .maxX = Integer.MinValue, _
                                            .miny = Integer.MaxValue, .maxy = Integer.MinValue}
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID(0) = LocID Then
                    b.minX = Math.Min(b.minX, x)
                    b.minY = Math.Min(b.minY, y)
                    b.maxX = Math.Max(b.maxX, x)
                    b.maxY = Math.Max(b.maxY, y)
                End If
            Next x
        Next y

        Dim xSize As Integer = b.maxX - b.minX
        Dim ySize As Integer = b.maxY - b.minY
        Dim isPossiblePoint(xSize, ySize) As Boolean
        Dim LPos As New Point(b.minX, b.minY)

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If m.board(x + LPos.X, y + LPos.Y).locID(0) = LocID _
                And Not m.board(x + LPos.X, y + LPos.Y).passability.isPenetrable _
                And Not m.board(x + LPos.X, y + LPos.Y).passability.isPass _
                And Not m.board(x + LPos.X, y + LPos.Y).passability.isAttended _
                And Not m.board(x + LPos.X, y + LPos.Y).passability.isBorder Then
                    isPossiblePoint(x, y) = True
                End If
            Next x
        Next y
        If m.symmID > -1 Then
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isPossiblePoint(x, y) Then
                        Dim p() As Point = symm.ApplySymm(New Point(x + b.minX, y + LPos.Y), settMap.nRaces, m, CInt(Math.Ceiling(settLoc.minStackToStackDist)))
                        If p.Length = settMap.nRaces Then
                            For k As Integer = 0 To UBound(p) Step 1
                                Dim tx As Integer = p(k).X - LPos.X
                                Dim ty As Integer = p(k).Y - LPos.Y
                                If tx >= 0 And ty >= 0 And tx <= xSize And ty <= ySize Then isPossiblePoint(tx, ty) = False
                            Next k
                            isPossiblePoint(x, y) = True
                        Else
                            isPossiblePoint(x, y) = False
                        End If
                    End If
                Next x
            Next y
        End If

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isPossiblePoint(x, y) Then
                    Dim t As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, xSize, ySize, CInt(Math.Ceiling(settLoc.minStackToStackDist)))
                    For j As Integer = t.minY To t.maxY Step 1
                        For i As Integer = t.minX To t.maxX Step 1
                            If m.board(i + LPos.X, j + LPos.Y).stack.GuardLoc _
                            AndAlso New Point(x, y).SqDist(i, j) < settLoc.minStackToStackDist * settLoc.minStackToStackDist Then
                                isPossiblePoint(x, y) = False
                            End If
                        Next i
                    Next j
                End If
            Next x
        Next y

        Dim corners As New List(Of Point)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                    For j As Integer = 0 To 1 Step 1
                        For i As Integer = 0 To 1 Step 1
                            corners.Add(New Point(x + i * (genmap.ActiveObjects(DefMapObjects.Types.Capital).Size - 1), _
                                                  y + j * (genmap.ActiveObjects(DefMapObjects.Types.Capital).Size - 1)))
                        Next i
                    Next j
                End If
            Next x
        Next y
        If corners.Count > 0 Then
            Dim minSqDist As Integer = CInt(3.2 ^ 2)
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isPossiblePoint(x, y) Then
                        For Each p As Point In corners
                            If p.SqDist(x + b.minX, y + b.minY) < minSqDist Then
                                isPossiblePoint(x, y) = False
                                Exit For
                            End If
                        Next p
                    End If
                Next x
            Next y
        End If

        Dim StackPos(Math.Max(24, Environment.ProcessorCount * 6) - 1) As List(Of Point)
        Dim rms(UBound(StackPos)) As Double
        Dim r(), rcut, averR As Double
        Dim tryagain As Boolean
        Dim selected As Integer = 0
        r = Nothing

        For i As Integer = 0 To UBound(StackPos) Step 1
            StackPos(i) = PlaceStacks(isPossiblePoint, settLoc)
            If StackPos(i).Count > 1 Then
                tryagain = True
                rcut = 0
                Do While tryagain
                    tryagain = False
                    ReDim r(StackPos(i).Count - 1)
                    rcut += settLoc.minStackToStackDist
                    For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                        For q As Integer = 0 To StackPos(i).Count - 1 Step 1
                            If Not p = q Then
                                Dim d As Integer = StackPos(i)(p).SqDist(StackPos(i)(q))
                                If d <= rcut * rcut Then r(p) += Math.Sqrt(d)
                            End If
                        Next q
                    Next p
                    For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                        If r(p) = 0 Then
                            tryagain = True
                            Exit For
                        End If
                    Next p
                Loop
                averR = 0
                For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                    averR += r(p)
                Next p
                averR /= (StackPos(i).Count - 1)
                For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                    rms(i) += (r(p) - averR) ^ 2
                Next p
                rms(i) = Math.Sqrt(rms(i) / (StackPos(i).Count * (StackPos(i).Count - 1))) / rcut
            ElseIf StackPos(i).Count = 1 Then
                rms(i) = 1
            Else
                rms(i) = Double.MaxValue / 10
            End If
            If rms(i) < rms(selected) Then selected = i
        Next i
        Dim res As New List(Of Point)
        For Each p As Point In StackPos(selected)
            res.Add(New Point(p.X + LPos.X, p.Y + LPos.Y))
        Next p
        Return res
    End Function
    Private Function PlaceStacks(ByRef isPossiblePoint(,) As Boolean, ByRef settLoc As Map.SettingsLoc) As List(Of Point)
        Dim tolerance As Integer = CInt(Math.Ceiling(settLoc.minStackToStackDist))
        Dim minDistSq As Integer = CInt(Math.Ceiling(settLoc.minStackToStackDist * settLoc.minStackToStackDist))
        Dim xSize As Integer = UBound(isPossiblePoint, 1)
        Dim ySize As Integer = UBound(isPossiblePoint, 2)
        Dim PossiblePoints(isPossiblePoint.Length - 1) As Point
        Dim PosPID(xSize, ySize) As Integer
        Dim IDs As New List(Of Integer)
        Dim n As Integer = -1
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isPossiblePoint(x, y) Then
                    n += 1
                    PossiblePoints(n) = New Point(x, y)
                    PosPID(x, y) = n
                    IDs.Add(n)
                Else
                    PosPID(x, y) = -1
                End If
            Next x
        Next y
        ReDim Preserve PossiblePoints(n)
        Dim output As New List(Of Point)
        Do While IDs.Count > 0
            Dim r As Integer = comm.RandomSelection(IDs, False)
            output.Add(PossiblePoints(r))
            IDs.Remove(r)
            PosPID(PossiblePoints(r).X, PossiblePoints(r).Y) = -1
            Dim t As Location.Borders = ImpenetrableMeshGen.NearestXY(PossiblePoints(r).X, PossiblePoints(r).Y, xSize, ySize, tolerance)
            For j As Integer = t.minY To t.maxY Step 1
                For i As Integer = t.minX To t.maxX Step 1
                    If PosPID(i, j) > -1 AndAlso PossiblePoints(r).SqDist(i, j) < minDistSq Then
                        IDs.Remove(PosPID(i, j))
                        PosPID(i, j) = -1
                    End If
                Next i
            Next j
        Loop
        Return output
    End Function

    Protected Friend Function PlasePassesGuards(ByVal m As Map, ByRef settMap As Map.SettingsMap, _
                                                ByVal unmarkPassagesWithoutGuard As Boolean, _
                                                ByRef term As TerminationCondition) As Point()()
        If Not settMap.AddGuardsBetweenLocations Then Return Nothing
        Dim passTile(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                passTile(x, y) = m.board(x, y).passability.isPass
            Next x
        Next y
        Dim connected()(,) As Boolean = GetConnected(passTile)
        Dim passages(UBound(connected)) As PassageGuardPlacer.Passage
        Parallel.For(0, connected.Length, _
         Sub(i As Integer)

             Call passages(i).InitializeEdgePointsArrays(UBound(m.Loc))

             Dim minx As Integer = Integer.MaxValue
             Dim maxx As Integer = Integer.MinValue
             Dim miny As Integer = Integer.MaxValue
             Dim maxy As Integer = Integer.MinValue
             For y As Integer = 0 To m.ySize Step 1
                 For x As Integer = 0 To m.xSize Step 1
                     If connected(i)(x, y) Then
                         minx = Math.Min(minx, x)
                         maxx = Math.Max(maxx, x)
                         miny = Math.Min(miny, y)
                         maxy = Math.Max(maxy, y)
                     End If
                 Next x
             Next y
             minx = Math.Max(minx - 1, 0)
             maxx = Math.Min(maxx + 1, m.xSize)
             miny = Math.Max(miny - 1, 0)
             maxy = Math.Min(maxy + 1, m.ySize)
             ReDim passages(i).passTiles(maxx - minx, maxy - miny)
             Dim pointID As Integer = -1
             For y As Integer = miny To maxy Step 1
                 For x As Integer = minx To maxx Step 1
                     If connected(i)(x, y) Then
                         pointID += 1
                         passages(i).passTiles(x - minx, y - miny) = pointID
                     Else
                         passages(i).passTiles(x - minx, y - miny) = -1
                     End If
                     If Not connected(i)(x, y) And Not m.board(x, y).passability.isBorder And Not m.board(x, y).passability.isAttended Then
                         Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
                         For q As Integer = b.minY To b.maxY Step 1
                             For p As Integer = b.minX To b.maxX Step 1
                                 If connected(i)(p, q) And Not m.board(x, y).passability.isBorder And Not m.board(x, y).passability.isAttended Then
                                     'passages(i).edgePoints(m.board(x, y).locID(0) - 1).Add(New Point(x - minx, y - miny))
                                     Call passages(i).edges(m.board(x, y).locID(0) - 1).AddPoint(x - minx, y - miny)
                                     p = b.maxX
                                     q = b.maxY
                                 End If
                             Next p
                         Next q
                     End If
                 Next x
             Next y
             passages(i).Size = New Point(maxx - minx, maxy - miny)
             passages(i).bias = New Point(minx, miny)
         End Sub)

        If m.symmID > -1 Then
            Dim s()() As Point
            Dim del, pointFound As Boolean
            For i As Integer = 0 To UBound(passages) - 1 Step 1
                If Not IsNothing(passages(i).passTiles) Then
                    ReDim s(passages(i).passTiles.Length - 1)
                    For y As Integer = 0 To passages(i).Size.Y Step 1
                        For x As Integer = 0 To passages(i).Size.X Step 1
                            If passages(i).passTiles(x, y) > -1 Then
                                s(passages(i).passTiles(x, y)) = symm.ApplySymm(New Point(passages(i).bias.X + x, passages(i).bias.Y + y), settMap.nRaces, m, 1)
                            End If
                        Next x
                    Next y

                    For j As Integer = i + 1 To UBound(passages) Step 1
                        If Not IsNothing(passages(j).passTiles) Then
                            del = True
                            For y As Integer = 0 To passages(j).Size.Y Step 1
                                For x As Integer = 0 To passages(j).Size.X Step 1
                                    If passages(j).passTiles(x, y) > -1 Then
                                        pointFound = False
                                        For t As Integer = 0 To UBound(s) Step 1
                                            If Not IsNothing(s(t)) Then
                                                For q As Integer = 0 To UBound(s(t)) Step 1
                                                    If x + passages(j).bias.X = s(t)(q).X And y + passages(j).bias.Y = s(t)(q).Y Then
                                                        pointFound = True
                                                        Exit For
                                                    End If
                                                Next q
                                                If pointFound Then Exit For
                                            End If
                                        Next t
                                        If Not pointFound Then
                                            del = False
                                            Exit For
                                        End If
                                    End If
                                Next x
                                If Not del Then Exit For
                            Next y
                            If del Then passages(j).passTiles = Nothing
                        End If
                    Next j
                End If
            Next i
        End If

        Dim result(UBound(passages))() As Point
        Dim k As Integer = -1
        For i As Integer = 0 To UBound(passages) Step 1
            If Not IsNothing(passages(i).passTiles) Then k += 1
        Next i
        Dim handle(k) As Integer
        k = -1
        For i As Integer = 0 To UBound(passages) Step 1
            If Not IsNothing(passages(i).passTiles) Then
                k += 1
                handle(k) = i
            End If
        Next i

        Dim termL As TerminationCondition = term
        Parallel.For(0, k + 1, _
         Sub(i As Integer)
             result(handle(i)) = HandlePath(m, passages(handle(i)), unmarkPassagesWithoutGuard, termL)
         End Sub)
        term = termL

        If unmarkPassagesWithoutGuard Then
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If m.board(x, y).passability.isPass Then
                        m.board(x, y).passability.isPass = False
                        m.board(x, y).passability.isPenetrable = False
                    End If
                Next x
            Next y
            For i As Integer = 0 To UBound(passages) Step 1
                If Not IsNothing(passages(i).passTiles) Then
                    Dim setAsPass As Boolean = Not IsNothing(result(i))
                    For y As Integer = 0 To passages(i).Size.Y Step 1
                        For x As Integer = 0 To passages(i).Size.X Step 1
                            If passages(i).passTiles(x, y) > -1 Then
                                Dim xb As Integer = passages(i).bias.X + x
                                Dim yb As Integer = passages(i).bias.Y + y
                                If m.symmID > -1 Then
                                    Dim p() As Point = symm.ApplySymm(New Point(xb, yb), settMap.nRaces, m, 1)
                                    For r As Integer = 0 To UBound(p) Step 1
                                        m.board(p(r).X, p(r).Y).passability.isPass = setAsPass
                                        m.board(p(r).X, p(r).Y).passability.isPenetrable = setAsPass
                                    Next r
                                Else
                                    m.board(xb, yb).passability.isPass = setAsPass
                                    m.board(xb, yb).passability.isPenetrable = setAsPass
                                End If
                            End If
                        Next x
                    Next y
                End If
            Next i
            Return Nothing
        Else
            Return result
        End If
    End Function
    Private Function HandlePath(ByRef m As Map, ByRef path As PassageGuardPlacer.Passage, _
                                ByRef justCheckGuardNecessity As Boolean, _
                                ByRef term As TerminationCondition) As Point()

        term.CheckTime()
        If term.ExitFromLoops Then Return Nothing

        Dim n As Integer = -1
        For j As Integer = 0 To path.Size.Y Step 1
            For i As Integer = 0 To path.Size.X Step 1
                If path.passTiles(i, j) > -1 Then n = Math.Max(n, path.passTiles(i, j))
            Next i
        Next j
        Dim gPlacer As PassageGuardPlacer = Nothing

        For currentNLimit As Integer = 0 To n Step 1
            gPlacer = New PassageGuardPlacer(currentNLimit, term, path, justCheckGuardNecessity)
            Call gPlacer.PlaceGuardLoc(-1, -1)
            If term.ExitFromLoops And gPlacer.debugTestsRun > 100 Then
                m.log.Add("Guards: " & currentNLimit + 1 & " Tests: " & gPlacer.debugTestsRun & " simpFilter: " & gPlacer.debugSimpleFilter & " passage size: " & gPlacer.pointsList.totalCount)
            End If
            If gPlacer.bestN > Integer.MinValue Then Exit For
        Next currentNLimit


        If gPlacer.bestN < 0 Then Return Nothing

        Dim out() As Point
        If Not justCheckGuardNecessity Then
            ReDim out(gPlacer.bestN)
            For i As Integer = 0 To gPlacer.bestN Step 1
                out(i) = New Point(gPlacer.pointsList.content(gPlacer.bestOutput(i)).pos.X + path.bias.X, _
                                   gPlacer.pointsList.content(gPlacer.bestOutput(i)).pos.Y + path.bias.Y)
            Next i
        Else
            out = {New Point(-1, -1)}
        End If
        Return out
    End Function
    Private Function GetConnected(ByRef free(,) As Boolean) As Boolean()(,)
        Dim connected()(,) As Boolean = Nothing
        Dim p As Point = New Point(-1, -1)
        Do While Not IsNothing(p)
            p = genmap.FindDisconnected(free, connected)
            If Not IsNothing(p) Then
                If IsNothing(connected) Then
                    ReDim connected(0)
                Else
                    ReDim Preserve connected(connected.Length)
                End If
                connected(UBound(connected)) = genmap.FindConnected(free, p)
            End If
        Loop
        Return connected
    End Function

    Class PassageGuardPlacer

        '''лучшее положение гвардов
        Public bestOutput() As Integer
        '''минимальное количество гвардов, которыми закрыли проход
        Public bestN As Integer = Integer.MinValue
        '''максимальное количество гвардов
        Private ReadOnly currentNLimit As Integer
        '''лимит времени на задачу
        Private term As TerminationCondition
        ' '''проверенные положения гвардов
        'Private checkedPositions As New List(Of String)
        '''список точек прохода
        Public ReadOnly pointsList As Passage.DisabablePointsArray
        '''просто проверит, нужны ли вообще гварды
        Private ReadOnly justCheckGuardNecessity As Boolean

        Private ReadOnly currentIDs() As Integer

        Public debugTestsRun As Integer
        Public debugSimpleFilter As Integer
        Public debugTestPassTime As Integer

        Private goPass As Passage

        Private ReadOnly guardBorderPoints() As Point

        Private ReadOnly disableWhenSelected(,) As List(Of Point)
        Private ReadOnly disableEdgePointOnSelection(,) As Boolean
        Private ReadOnly disableEdgePointOnSelectionList(,)() As List(Of Integer)

        Private ReadOnly bordersT1(,) As Location.Borders

        ''' <summary>Отправляй в качестве инпута в поле IDs</summary>
        Private currentOutput() As Integer

        Public Structure Passage
            Dim bias, Size As Point
            Dim passTiles(,) As Integer

            Dim edgesCount As Integer
            Dim edges() As DisabablePointsArray

            Public Structure DisabablePointsArray

                Dim content() As EPoint
                Dim totalCount As Integer
                Dim enabledCount As Integer

                Dim disabledAtStep() As Disabled

                Public Structure Disabled
                    Dim list() As Integer
                    Dim count As Integer
                End Structure

                Public Structure EPoint
                    Dim pos As Point
                    Dim enabled As Boolean

                    Public Sub New(ByRef p As Point)
                        pos = p
                        enabled = True
                    End Sub
                End Structure

                Public Sub InitializeContentList(Optional ByVal size As Integer = -1)
                    ReDim content(size)
                    enabledCount = size
                    totalCount = size
                End Sub

                Public Sub InitializeDisabledList(ByRef stepsCount As Integer)
                    ReDim disabledAtStep(stepsCount)
                    For i As Integer = 0 To stepsCount Step 1
                        ReDim disabledAtStep(i).list(totalCount)
                        For j As Integer = 0 To totalCount Step 1
                            disabledAtStep(i).list(j) = -1
                        Next j
                        disabledAtStep(i).count = -1
                    Next i
                End Sub

                Public Sub AddPoint(ByRef x As Integer, ByRef y As Integer)
                    Call AddPoint(New Point(x, y))
                End Sub
                Public Sub AddPoint(ByRef p As Point)
                    totalCount += 1
                    enabledCount += 1
                    ReDim Preserve content(totalCount)
                    Call AddPoint(p, totalCount)
                End Sub
                Public Sub AddPoint(ByRef x As Integer, ByRef y As Integer, ByRef pos As Integer)
                    Call AddPoint(New Point(x, y), pos)
                End Sub
                Public Sub AddPoint(ByRef p As Point, ByRef pos As Integer)
                    content(pos) = New DisabablePointsArray.EPoint(p)
                End Sub

                Public Sub EnableAllAtStep(ByRef currentStep As Integer)
                    For i As Integer = 0 To disabledAtStep(currentStep).count Step 1
                        enabledCount += 1
                        content(disabledAtStep(currentStep).list(i)).enabled = True
                        disabledAtStep(currentStep).list(i) = -1
                    Next i
                    disabledAtStep(currentStep).count = -1
                End Sub
                Public Sub Disable(ByRef currentStep As Integer, ByRef n As Integer)
                    If Not content(n).enabled Then Exit Sub
                    enabledCount -= 1
                    content(n).enabled = False
                    disabledAtStep(currentStep).count += 1
                    disabledAtStep(currentStep).list(disabledAtStep(currentStep).count) = n
                End Sub
            End Structure

            Public Sub InitializeEdgePointsArrays(ByRef eCount As Integer)
                edgesCount = eCount
                ReDim edges(eCount)
                For i As Integer = 0 To eCount Step 1
                    Call edges(i).InitializeContentList()
                Next i
            End Sub
        End Structure

        Public Shared Sub speedBanchmark()

            Dim size As Integer = 76
            Dim pwidth As Integer = 4

            Dim passage As New Passage With {.bias = New Point(0, 0), .Size = New Point(size, size)}
            ReDim passage.passTiles(size, size)
            Call passage.InitializeEdgePointsArrays(3)

            Dim pmin As Integer = CInt(size / 2 - (pwidth - 1) / 2)
            Dim pmax As Integer = CInt(size / 2 + (pwidth - 1) / 2)
            Dim n As Integer = -1
            For i As Integer = 0 To size Step 1
                For j As Integer = 0 To size Step 1
                    If i = 0 Or i = size Or j = 0 Or j = size Then
                        passage.passTiles(i, j) = -1
                        If (i >= pmin And i <= pmax) Or (j >= pmin And j <= pmax) Then
                            If j = 0 Then
                                Call passage.edges(0).AddPoint(i, j)
                            ElseIf j = size Then
                                Call passage.edges(1).AddPoint(i, j)
                            ElseIf i = 0 Then
                                Call passage.edges(2).AddPoint(i, j)
                            ElseIf i = size Then
                                Call passage.edges(3).AddPoint(i, j)
                            End If
                        End If
                    Else
                        If (i >= pmin And i <= pmax) Or (j >= pmin And j <= pmax) Then
                            n += 1
                            passage.passTiles(i, j) = n
                        ElseIf i >= pmin - pwidth And i <= pmax + pwidth And j >= pmin - pwidth And j <= pmax + pwidth Then
                            n += 1
                            passage.passTiles(i, j) = n
                        Else
                            passage.passTiles(i, j) = -1
                        End If
                    End If
                Next j
            Next i
            'For j As Integer = 0 To passage.Size.Y Step 1
            '    For i As Integer = 0 To passage.Size.X Step 1
            '        If passage.passTiles(i, j) > -1 Then
            '            Console.Write(passage.passTiles(i, j) & vbTab)
            '        Else
            '            Dim isedge As Boolean = False
            '            For k As Integer = 0 To passage.edgesCount Step 1
            '                For p As Integer = 0 To passage.edges(k).totalCount Step 1
            '                    If passage.edges(k).content(p).pos.X = i And passage.edges(k).content(p).pos.Y = j Then
            '                        isedge = True
            '                        Exit For
            '                    End If
            '                Next p
            '                If isedge Then Exit For
            '            Next k
            '            If isedge Then
            '                Console.Write(-22 & vbTab)
            '            Else
            '                Console.Write(passage.passTiles(i, j) & vbTab)
            '            End If
            '        End If
            '    Next i
            '    Console.Write(vbNewLine)
            'Next j

            Dim pgp As New PassageGuardPlacer(7, New TerminationCondition(10000), passage, False, 123456)
            Dim t0 As Integer = Environment.TickCount
            Call pgp.PlaceGuardLoc(-1, -1)

            Console.WriteLine("' " & pgp.debugSimpleFilter & vbTab & pgp.debugTestsRun)
            Console.WriteLine("' " & pgp.bestN)
            Console.WriteLine("' " & Environment.TickCount - t0 & vbTab & pgp.debugTestPassTime)
            ' 49	35801
            ' -2147483648
            ' 10015	9267
        End Sub

        Public Sub New(ByRef limit As Integer, ByRef t As TerminationCondition, _
                       ByRef path As PassageGuardPlacer.Passage, _
                       ByRef checkGuardsNecessity As Boolean, _
                       Optional ByVal seed As Integer = -1)
            currentNLimit = limit
            term = t
            goPass = path
            Dim IDs_UpperBound As Integer = -1
            justCheckGuardNecessity = checkGuardsNecessity

            For j As Integer = 0 To path.Size.Y Step 1
                For i As Integer = 0 To path.Size.X Step 1
                    If path.passTiles(i, j) > -1 Then IDs_UpperBound += 1
                Next i
            Next j
            Call pointsList.InitializeContentList(IDs_UpperBound)
            Call pointsList.InitializeDisabledList(currentNLimit)
            ReDim currentIDs(IDs_UpperBound)

            For j As Integer = 0 To path.Size.Y Step 1
                For i As Integer = 0 To path.Size.X Step 1
                    If path.passTiles(i, j) > -1 Then
                        Call pointsList.AddPoint(i, j, path.passTiles(i, j))
                        currentIDs(path.passTiles(i, j)) = path.passTiles(i, j)
                    End If
                Next i
            Next j

            ReDim bestOutput(IDs_UpperBound), currentOutput(IDs_UpperBound)
            For i As Integer = 0 To IDs_UpperBound Step 1
                bestOutput(i) = -1
                currentOutput(i) = -1
            Next i

            ReDim guardBorderPoints(5 * 5 - 3 * 3 - 1)
            Dim d As Integer = 2
            Dim n As Integer = -1
            For i As Integer = -1 To 2 Step 1
                n += 1
                guardBorderPoints(n) = New Point(-d, i)
            Next i
            For i As Integer = -1 To 2 Step 1
                n += 1
                guardBorderPoints(n) = New Point(i, d)
            Next i
            For i As Integer = 1 To -2 Step -1
                n += 1
                guardBorderPoints(n) = New Point(d, i)
            Next i
            For i As Integer = 1 To -2 Step -1
                n += 1
                guardBorderPoints(n) = New Point(i, -d)
            Next i

            If IDs_UpperBound > 0 Then
                Dim rnd As New RndValueGen(seed)
                Call rnd.Shuffle(currentIDs)
            End If

            ReDim disableWhenSelected(path.Size.X, path.Size.Y), _
                  disableEdgePointOnSelection(path.Size.X, path.Size.Y), _
                  disableEdgePointOnSelectionList(path.Size.X, path.Size.Y), _
                  bordersT1(path.Size.X, path.Size.Y)
            For y As Integer = 0 To path.Size.Y Step 1
                For x As Integer = 0 To path.Size.X Step 1
                    If path.passTiles(x, y) > -1 Then
                        ReDim disableEdgePointOnSelectionList(x, y)(path.edgesCount)
                        For i = 0 To path.edgesCount Step 1
                            disableEdgePointOnSelectionList(x, y)(i) = New List(Of Integer)
                        Next i
                        disableWhenSelected(x, y) = New List(Of Point)
                        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, path.Size.X, path.Size.Y, 1)
                        For j As Integer = b.minY To b.maxY Step 1
                            For i As Integer = b.minX To b.maxX Step 1
                                If path.passTiles(i, j) > -1 Then disableWhenSelected(x, y).Add(New Point(i, j))
                            Next i
                        Next j

                        For i As Integer = 0 To path.edgesCount Step 1
                            If path.edges(i).totalCount > 0 Then
                                For j As Integer = 0 To path.edges(i).totalCount Step 1
                                    If Math.Abs(path.edges(i).content(j).pos.X - x) <= 1 _
                                      AndAlso Math.Abs(path.edges(i).content(j).pos.Y - y) <= 1 Then
                                        disableEdgePointOnSelection(x, y) = True
                                        disableEdgePointOnSelectionList(x, y)(i).Add(j)
                                    End If
                                Next j
                            End If
                        Next i
                    End If
                    bordersT1(x, y) = ImpenetrableMeshGen.NearestXY(x, y, path.Size.X, path.Size.Y, 1)
                Next x
            Next y

            For i As Integer = 0 To path.edgesCount Step 1
                Call path.edges(i).InitializeDisabledList(currentNLimit)
            Next i
        End Sub

        ''' <param name="currentN">количество поставленных гвардов - 1</param>
        ''' <param name="selected_I">id выбранной точки</param>
        Protected Friend Sub PlaceGuardLoc(ByVal currentN As Integer, _
                                           ByRef selected_I As Integer)
            term.CheckTime()
            If term.ExitFromLoops Then
                Exit Sub
            End If

            If selected_I > -1 Then
                'Если здесь гвардов >= гвардов в лучшем результате, то выходим
                If currentN >= bestN And bestN > Integer.MinValue Then Exit Sub

                Dim selPoint As Point = pointsList.content(currentIDs(selected_I)).pos
                currentOutput(currentN) = currentIDs(selected_I)

                Call pointsList.EnableAllAtStep(currentN)
                For i As Integer = 0 To goPass.edgesCount Step 1
                    Call goPass.edges(i).EnableAllAtStep(currentN)
                Next i

                For Each p As Point In disableWhenSelected(selPoint.X, selPoint.Y)
                    Call pointsList.Disable(currentN, goPass.passTiles(p.X, p.Y))
                Next p
                If disableEdgePointOnSelection(selPoint.X, selPoint.Y) Then
                    For i As Integer = 0 To goPass.edgesCount Step 1
                        For Each p As Integer In disableEdgePointOnSelectionList(selPoint.X, selPoint.Y)(i)
                            Call goPass.edges(i).Disable(currentN, p)
                        Next p
                    Next i
                End If
            End If

            If currentN = currentNLimit Or justCheckGuardNecessity Then
                If TestPassage(pointsList.content, currentOutput) Then
                    If currentN = -1 Then
                        bestN = -1
                        Exit Sub
                    End If
                    For i As Integer = 0 To currentN Step 1
                        bestOutput(i) = currentOutput(i)
                    Next i
                    For i As Integer = currentN + 1 To bestN Step 1
                        bestOutput(i) = -1
                    Next i
                    bestN = currentN
                    Exit Sub
                ElseIf justCheckGuardNecessity Then
                    bestOutput = Nothing
                    currentOutput = Nothing
                    bestN = 1
                    Exit Sub
                Else
                    currentOutput(currentN) = -1
                    Exit Sub
                End If
            End If

            For i As Integer = selected_I + 1 To pointsList.totalCount Step 1
                If pointsList.content(currentIDs(i)).enabled Then
                    Call PlaceGuardLoc(currentN + 1, i)
                    If bestN > -1 Then Exit For
                End If
            Next i
            If currentN > -1 Then
                Call pointsList.EnableAllAtStep(currentN)
                For i As Integer = 0 To goPass.edgesCount Step 1
                    Call goPass.edges(i).EnableAllAtStep(currentN)
                Next i
            End If
        End Sub

        Private Function TestPassage(ByRef pointsList() As Passage.DisabablePointsArray.EPoint, ByRef currentOutput() As Integer) As Boolean

            If Not GuardPosFilter(pointsList, currentOutput) Then
                debugSimpleFilter += 1
                Return False
            End If

            debugTestsRun += 1

            Dim t0 As Integer = Environment.TickCount

            Dim n As Integer = 0
            For i As Integer = 0 To goPass.edgesCount Step 1
                If goPass.edges(i).enabledCount > -1 Then n += 1
            Next i
            If n > 1 Then
                Dim LocID(goPass.Size.X, goPass.Size.Y) As Integer
                For i As Integer = 0 To goPass.edgesCount Step 1
                    Dim i1 As Integer = i + 1
                    If goPass.edges(i).enabledCount > -1 Then
                        For r As Integer = 0 To goPass.edges(i).totalCount Step 1
                            If goPass.edges(i).content(r).enabled Then
                                For y As Integer = bordersT1(goPass.edges(i).content(r).pos.X, goPass.edges(i).content(r).pos.Y).minY To bordersT1(goPass.edges(i).content(r).pos.X, goPass.edges(i).content(r).pos.Y).maxY Step 1
                                    For x As Integer = bordersT1(goPass.edges(i).content(r).pos.X, goPass.edges(i).content(r).pos.Y).minX To bordersT1(goPass.edges(i).content(r).pos.X, goPass.edges(i).content(r).pos.Y).maxX Step 1
                                        If ShortIsTileEnabled(x, y) Then
                                            If LocID(x, y) = 0 Then
                                                LocID(x, y) = i1
                                            Else
                                                If Not LocID(x, y) = i1 Then
                                                    debugTestPassTime += Environment.TickCount - t0
                                                    Return False
                                                End If
                                            End If
                                        End If
                                    Next x
                                Next y
                            End If
                        Next r
                    End If
                Next i

                Dim connectedWithLoc(goPass.Size.X, goPass.Size.Y) As Integer
                Dim check(goPass.Size.X, goPass.Size.Y) As Boolean
                For y1 As Integer = 0 To goPass.Size.Y Step 1
                    For x1 As Integer = 0 To goPass.Size.X Step 1
                        If connectedWithLoc(x1, y1) = 0 AndAlso LocID(x1, y1) > 0 Then

                            check(x1, y1) = True

                            Dim r As Integer = 1
                            Dim restartX As Integer = 0
                            Dim restartY As Integer = 0
                            Dim x0 As Integer
                            Dim resetRestarY As Boolean
                            Do While r > 0
                                resetRestarY = True
                                For j As Integer = restartY To goPass.Size.Y Step 1
                                    For i As Integer = restartX To goPass.Size.X Step 1
                                        If check(i, j) Then
                                            For Each nearPoint As Point In disableWhenSelected(i, j)
                                                If ShortIsTileEnabled(nearPoint.X, nearPoint.Y) Then
                                                    If connectedWithLoc(nearPoint.X, nearPoint.Y) = 0 Then
                                                        If LocID(nearPoint.X, nearPoint.Y) > 0 And Not LocID(x1, y1) = LocID(nearPoint.X, nearPoint.Y) Then
                                                            debugTestPassTime += Environment.TickCount - t0
                                                            Return False
                                                        End If
                                                        connectedWithLoc(nearPoint.X, nearPoint.Y) = LocID(x1, y1)
                                                        If Not check(nearPoint.X, nearPoint.Y) Then
                                                            check(nearPoint.X, nearPoint.Y) = True
                                                            r += 1
                                                        End If
                                                    ElseIf Not connectedWithLoc(nearPoint.X, nearPoint.Y) = LocID(x1, y1) Then
                                                        debugTestPassTime += Environment.TickCount - t0
                                                        Return False
                                                    End If
                                                End If
                                            Next nearPoint
                                            check(i, j) = False
                                            r -= 1
                                            x0 = bordersT1(i, j).minX
                                            restartY = bordersT1(i, j).minY
                                            resetRestarY = False
                                        End If
                                    Next i
                                    restartX = 0
                                Next j
                                If resetRestarY Then
                                    x0 = 0
                                    restartY = 0
                                End If
                                restartX = x0
                            Loop
                        End If
                    Next x1
                Next y1
            End If
            debugTestPassTime += Environment.TickCount - t0
            Return True
        End Function
        Private Function GuardPosFilter(ByRef pointsList() As Passage.DisabablePointsArray.EPoint, ByRef currentOutput() As Integer) As Boolean

            Dim nGBP As Integer = UBound(guardBorderPoints)
            Dim nSegments As Integer

            For Each id As Integer In currentOutput
                If id > -1 Then
                    'если гуард перекрывает хоть одну точку границы, то он проходит тест
                    If Not disableEdgePointOnSelection(pointsList(id).pos.X, pointsList(id).pos.Y) Then
                        nSegments = 0
                        For i As Integer = 1 To nGBP Step 1
                            If IsTileEnabled(pointsList(id).pos, guardBorderPoints(i - 1)) _
                             AndAlso IsTileEnabled(pointsList(id).pos, guardBorderPoints(i)) Then
                                nSegments += 1
                            End If
                        Next i
                        If nSegments < 2 Then Return False
                    End If
                Else
                    Exit For
                End If
            Next id
            Return True
        End Function
        Private Function IsTileEnabled(ByRef pos As Point, ByRef bias As Point) As Boolean
            Return IsTileEnabled(pos.X + bias.X, pos.Y + bias.Y)
        End Function
        Private Function IsTileEnabled(ByRef x As Integer, ByRef y As Integer) As Boolean
            If x < 0 Then Return False
            If y < 0 Then Return False
            If x > UBound(goPass.passTiles, 1) Then Return False
            If y > UBound(goPass.passTiles, 2) Then Return False
            Return ShortIsTileEnabled(x, y)
        End Function
        Private Function ShortIsTileEnabled(ByRef x As Integer, ByRef y As Integer) As Boolean
            If goPass.passTiles(x, y) = -1 Then Return False
            Return pointsList.content(goPass.passTiles(x, y)).enabled
        End Function
        'Private Function NConnected(ByRef free(,) As Boolean, ByRef m As Map) As Integer
        '    Dim conn()(,) As Boolean = GetConnected(free)
        '    Dim n As Integer = 0
        '    For i As Integer = 0 To UBound(conn) Step 1
        '        For y As Integer = 0 To m.ySize Step 1
        '            For x As Integer = 0 To m.xSize Step 1
        '                If conn(i)(x, y) And Not m.board(x, y).passability.isPass Then
        '                    n += 1
        '                    x = m.xSize
        '                    y = m.ySize
        '                End If
        '            Next x
        '        Next y
        '    Next i
        '    Return n
        'End Function

    End Class

    Private Function ConvertPointsArray(ByRef guards()()() As Point) As Point()
        If IsNothing(guards) Then Return Nothing
        Dim n As Integer = -1
        For i As Integer = 0 To UBound(guards) Step 1
            If Not IsNothing(guards(i)) Then
                For j As Integer = 0 To UBound(guards(i)) Step 1
                    If Not IsNothing(guards(i)(j)) Then n += guards(i)(j).Length
                Next j
            End If
        Next i
        If n = -1 Then Return Nothing
        Dim res(n) As Point
        n = -1
        For i As Integer = 0 To UBound(guards) Step 1
            If Not IsNothing(guards(i)) Then
                For j As Integer = 0 To UBound(guards(i)) Step 1
                    If Not IsNothing(guards(i)(j)) Then
                        For k As Integer = 0 To UBound(guards(i)(j)) Step 1
                            n += 1
                            res(n) = guards(i)(j)(k)
                        Next k
                    End If
                Next j
            End If
        Next i
        Return res
    End Function
    'Private Sub RemoveExcessPassGuards(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
    '                                   ByRef guards() As Point)
    '    If IsNothing(guards) Then Exit Sub
    '    Dim free(m.xSize, m.ySize) As Boolean
    '    For y As Integer = 0 To m.ySize Step 1
    '        For x As Integer = 0 To m.xSize Step 1
    '            If Not m.board(x, y).passability.isBorder And Not m.board(x, y).passability.isAttended Then free(x, y) = True
    '        Next x
    '    Next y
    '    Dim n As Integer
    '    Dim excluded As New List(Of Integer)
    '    Dim nLonsInFullyClosedState As Integer = CalcNDisconnectedLocs(free, m, settMap, guards, excluded)
    '
    '    For i As Integer = 0 To UBound(guards) Step 1
    '        excluded.Add(i)
    '        n = CalcNDisconnectedLocs(free, m, settMap, guards, excluded)
    '        If nLonsInFullyClosedState > n Then excluded.Remove(i)
    '    Next i
    '    If guards.Length = excluded.Count Then
    '        guards = Nothing
    '        Exit Sub
    '    End If
    '    Dim res(UBound(guards) - excluded.Count) As Point
    '    n = -1
    '    For i As Integer = 0 To UBound(guards) Step 1
    '        If Not excluded.Contains(i) Then
    '            n += 1
    '            res(n) = New Point(guards(i).X, guards(i).Y)
    '        End If
    '    Next i
    '    guards = res
    'End Sub
    'Private Function CalcNDisconnectedLocs(ByRef free(,) As Boolean, ByRef m As Map, _
    '                                       ByRef settMap As Map.SettingsMap, _
    '                                       ByRef guards() As Point, ByRef excluded As List(Of Integer)) As Integer
    '    Dim freeInClosedState(,) As Boolean = CType(free.Clone, Boolean(,))
    '
    '    For id As Integer = 0 To UBound(guards) Step 1
    '        If Not excluded.Contains(id) Then
    '            Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(guards(id), m, 1)
    '            For x As Integer = b.minX To b.maxX Step 1
    '                For y As Integer = b.minY To b.maxY Step 1
    '                    If m.symmID > -1 Then
    '                        Dim pp() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 0)
    '                        For Each item As Point In pp
    '                            freeInClosedState(item.X, item.Y) = False
    '                        Next item
    '                    Else
    '                        freeInClosedState(x, y) = False
    '                    End If
    '                Next y
    '            Next x
    '        End If
    '    Next id
    '
    '    Return NConnected(freeInClosedState, m)
    'End Function

    Private Sub PlacePassGuards(ByRef m As Map, ByRef guards() As Point, ByRef GroupID As Integer, ByRef settMap As Map.SettingsMap)
        If IsNothing(guards) Then Exit Sub
        For Each p As Point In guards
            GroupID += 1
            If m.symmID > -1 Then
                Dim pp() As Point = symm.ApplySymm(p, settMap.nRaces, m, 1)
                For Each item As Point In pp
                    m.board(item.X, item.Y).groupID = GroupID
                    m.board(item.X, item.Y).stack.PassGuardLoc = True
                Next item
            Else
                m.board(p.X, p.Y).groupID = GroupID
                m.board(p.X, p.Y).stack.PassGuardLoc = True
            End If
        Next p
    End Sub
End Class

Public Class Point
    ''' <summary>Координата по X</summary>
    Public X As Integer
    ''' <summary>Координата по Y</summary>
    Public Y As Integer
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Sub New(ByVal X As Integer, ByVal Y As Integer)
        Me.X = X
        Me.Y = Y
    End Sub

    ''' <summary>Возвращает квадрат расстояния до точки</summary>
    ''' <param name="p">Координаты точки</param>
    Public Function SqDist(ByRef p As Point) As Integer
        Return SqDist(p.X, p.Y)
    End Function
    ''' <summary>Возвращает квадрат расстояния до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function SqDist(ByRef x As Integer, ByRef y As Integer) As Integer
        Dim dx As Integer = Me.X - x
        Dim dy As Integer = Me.Y - y
        Return dx * dx + dy * dy
    End Function
    ''' <summary>Возвращает квадрат расстояния до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function SqDist(ByRef x As Double, ByRef y As Double) As Double
        Dim dx As Double = CDbl(Me.X) - x
        Dim dy As Double = CDbl(Me.Y) - y
        Return dx * dx + dy * dy
    End Function
    ''' <summary>Возвращает расстояние до точки</summary>
    ''' <param name="p">Координаты точки</param>
    Public Function Dist(ByRef p As Point) As Double
        Return Dist(p.X, p.Y)
    End Function
    ''' <summary>Возвращает расстояние до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function Dist(ByRef x As Integer, ByRef y As Integer) As Double
        Return Math.Sqrt(SqDist(x, y))
    End Function
    ''' <summary>Возвращает расстояние до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function Dist(ByRef x As Double, ByRef y As Double) As Double
        Return Math.Sqrt(SqDist(x, y))
    End Function

    Public Shared Function ToCharStr(ByRef p As Point) As String
        Return p.X & "_" & p.Y
    End Function
    Public Shared Function ToByteStr(ByRef p As Point) As String
        Return ToByteStr(p.X) & ToByteStr(p.Y)
    End Function
    Public Shared Function ToByteStr(ByRef v As Integer) As String
        Dim chars() As Byte = BitConverter.GetBytes(v)
        Dim r As String = ""
        For i As Integer = 0 To UBound(chars) Step 1
            r &= Chr(chars(i))
        Next i
        Return r
    End Function

End Class

Public Class WaterGen

    Public Const PlaceWaterMode As Integer = 2
    Public ReadOnly minLocationRadiusAtAll As Double
    Public Sub New()
        minLocationRadiusAtAll = 2
    End Sub

    Private rndgen As New RndValueGen
    Private comm As Common
    Private symm As New SymmetryOperations
    Private imp As ImpenetrableMeshGen
    Private wpCommon As WaterPlacer_Common

    Public waterAttendedObj As New Dictionary(Of DefMapObjects.Types, List(Of Integer))

    '''<summary>Сгенерирует озера на карте</summary>
    ''' <param name="m">Карта с сгенерированной силой отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef CustomBuildingRace() As String, ByRef impMG As ImpenetrableMeshGen)

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")

        If Not m.complited.StacksDesiredStatsGen_Done Then
            Throw New Exception("Сначала нужно выполнить StackPowerGen.Gen")
        End If

        comm = m.comm
        Call comm.ReadCustomBuildingRace()
        imp = impMG

        Dim t0 As Integer = Environment.TickCount

        Call MakeAttendedObjectsWithWaterPlacing()
        wpCommon = New WaterPlacer_Common(Me, m, settMap)

        If PlaceWaterMode = 1 Then
            Dim w As New WaterPlacer_1(Me, m, settMap)
            For i As Integer = 0 To m.Loc.Length - 1 Step 1
                If Not m.Loc(i).IsObtainedBySymmery Then Call w.PlaceWater(m.Loc(i))
            Next i
            Call w.Extend()
        ElseIf PlaceWaterMode = 2 Then
            Dim w As New WaterPlacer_2(Me, m, settMap)
            For i As Integer = 0 To m.Loc.Length - 1 Step 1
                If Not m.Loc(i).IsObtainedBySymmery Then Call w.PlaceWater(m.Loc(i))
            Next i
        Else
            MsgBox("Unexpected PlaceWaterMode" & PlaceWaterMode)
        End If
        m.complited.WaterCreation_Done = True
        Call m.log.Add("Water creation: " & Environment.TickCount - t0 & " ms")
    End Sub
    Private Sub MakeAttendedObjectsWithWaterPlacing()
        Dim r As Dictionary(Of String, DecorationPlacingProperties) = comm.objectRace

        Dim obj As New Dictionary(Of String, DefMapObjects.Types)
        obj.Add(GenDefaultValues.wObjKeyMerchant, DefMapObjects.Types.Vendor)
        obj.Add(GenDefaultValues.wObjKeyMercenaries, DefMapObjects.Types.Mercenary)
        obj.Add(GenDefaultValues.wObjKeyMage, DefMapObjects.Types.Mage)
        obj.Add(GenDefaultValues.wObjKeyTrainer, DefMapObjects.Types.Trainer)
        obj.Add(GenDefaultValues.wObjKeyRuin, DefMapObjects.Types.Ruins)

        Dim rObjList As List(Of String) = r.Keys.ToList
        Dim objList As List(Of String) = obj.Keys.ToList
        For Each k1 As String In robjList
            For Each k2 As String In objList
                If k1.ToLower.StartsWith(k2.ToLower) AndAlso r.Item(k1).water Then
                    If Not waterAttendedObj.ContainsKey(obj.Item(k2)) Then
                        waterAttendedObj.Add(obj.Item(k2), New List(Of Integer))
                    End If
                    For Each race As Integer In r.Item(k1).race
                        If Not waterAttendedObj.Item(obj.Item(k2)).Contains(race) Then
                            waterAttendedObj.Item(obj.Item(k2)).Add(race)
                        End If
                    Next race
                End If
            Next k2
        Next k1
    End Sub

    Public Class FreePointsInfo       
        ''' <summary>
        ''' True, если:
        '''  Not owner.wpCommon.HaveToBeGround
        '''  расстояние до обрабатываемой локации меньше или равно locationBorderTolerance.
        '''  по соседству есть хоть один тайл, который не занят непосещаемым объектом и может быть занят водой
        ''' </summary>
        Public isFree(,) As Boolean
        Public points() As Point
        Public IDs As List(Of Integer)
        Public xMax, yMax As Integer

        Public Sub New(ByRef freeCell(,) As Boolean)
            isFree = freeCell
            points = MakeFreePointsArray()
            IDs = MakeIDsList(0, UBound(points))
        End Sub

        Private Function MakeFreePointsArray() As Point()
            xMax = UBound(isFree, 1)
            yMax = UBound(isFree, 2)
            Dim n As Integer = -1
            For i As Integer = 0 To xMax Step 1
                For j As Integer = 0 To yMax Step 1
                    If isFree(i, j) Then n += 1
                Next j
            Next i
            Dim points(n) As Point
            n = -1
            For i As Integer = 0 To xMax Step 1
                For j As Integer = 0 To yMax Step 1
                    If isFree(i, j) Then
                        n += 1
                        points(n) = New Point(i, j)
                    End If
                Next j
            Next i
            Return points
        End Function
        Friend Shared Function MakeIDsList(ByVal minID As Integer, ByVal maxID As Integer) As List(Of Integer)
            Dim r As New List(Of Integer)
            For i As Integer = minID To maxID Step 1
                r.Add(i)
            Next i
            Return r
        End Function
    End Class
    Public Class WaterPlacer_Common
        Public owner As WaterGen
        Public m As Map
        Public settMap As Map.SettingsMap
        Public HaveToBeGround(,) As Boolean

        Public Sub New(ByRef w As WaterGen, ByRef myMap As Map, ByRef sM As Map.SettingsMap)
            owner = w
            m = myMap
            settMap = sM
            Call MakeHaveToBeGround()
        End Sub
        Private Sub MakeHaveToBeGround()
            ReDim HaveToBeGround(m.xSize, m.ySize)
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    'под всеми объектами и вокруг них не должно быть воды
                    'может быть проигнорировано для посещаемых объектов, для которых есть графика, предназначенная для воды
                    If m.board(i, j).passability.isAttended Or m.board(i, j).passability.isPenetrable Then HaveToBeGround(i, j) = True
                    If m.board(i, j).mapObject.objectID = DefMapObjects.Types.Mine Then
                        'вокруг шахт обязательно земля
                        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                HaveToBeGround(x, y) = True
                            Next y
                        Next x
                    End If
                    'под лесом и дорогами не должно быть воды
                    If m.board(i, j).surface.isForest Or m.board(i, j).surface.isRoad Then HaveToBeGround(i, j) = True
                Next j
            Next i
            If m.symmID > -1 Then
                'применяем операции симметрии, если надо
                For i As Integer = 0 To m.xSize Step 1
                    For j As Integer = 0 To m.ySize Step 1
                        If HaveToBeGround(i, j) Then
                            Dim pp() As Point = owner.symm.ApplySymm(New Point(i, j), settMap.nRaces, m, 1)
                            For k As Integer = 0 To UBound(pp) Step 1
                                Dim tx As Integer = pp(k).X
                                Dim ty As Integer = pp(k).Y
                                HaveToBeGround(tx, ty) = True
                            Next k
                        End If
                    Next j
                Next i
            End If
        End Sub

        Public Function WaterAmountCalc(ByRef loc As Location, ByRef freeCell(,) As Boolean, Optional ByVal locationBorderTolerance As Integer = 0) As Integer
            ReDim freeCell(m.xSize, m.ySize)
            Dim WaterAmount, Watered As Integer
            Dim locationFilter, surfaceFilter As Boolean
            Dim b As Location.Borders
            Dim p As Point
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    p = New Point(i, j)
                    locationFilter = False
                    b = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, locationBorderTolerance)
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            If m.board(x, y).locID(0) = loc.ID And Not owner.wpCommon.HaveToBeGround(x, y) AndAlso p.Dist(x, y) <= locationBorderTolerance Then
                                locationFilter = True
                                Exit For
                            End If
                        Next y
                        If locationFilter Then Exit For
                    Next x
                    If locationFilter Then
                        If m.board(i, j).passability.isBorder Then
                            surfaceFilter = False
                            b = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                            For x As Integer = b.minX To b.maxX Step 1
                                For y As Integer = b.minY To b.maxY Step 1
                                    If Not m.board(x, y).passability.isBorder And Not owner.wpCommon.HaveToBeGround(x, y) Then
                                        surfaceFilter = True
                                        Exit For
                                    End If
                                Next y
                                If surfaceFilter Then Exit For
                            Next x
                            If surfaceFilter Then
                                If m.board(i, j).locID(0) = loc.ID Then WaterAmount += 1
                                If m.board(i, j).surface.isWater Then
                                    If m.board(i, j).locID(0) = loc.ID Then Watered += 1
                                Else
                                    freeCell(i, j) = True
                                End If
                            End If
                        Else
                            If m.board(i, j).locID(0) = loc.ID Then WaterAmount += 1
                            If m.board(i, j).surface.isWater Then
                                If m.board(i, j).locID(0) = loc.ID Then Watered += 1
                            Else
                                freeCell(i, j) = True
                            End If
                        End If
                    End If
                Next j
            Next i
            WaterAmount = CInt(Math.Floor(WaterAmount * Math.Min(settMap.WaterAmount, 1))) - Watered
            Return WaterAmount
        End Function

        Public Function MakeWaterSufaceTestArray(ByRef pos As Point, ByRef w As WaterPlacer_2.WaterBlock) As Boolean(,)
            Dim result(m.xSize, m.ySize) As Boolean
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    result(x, y) = m.board(x, y).surface.isWater
                Next x
            Next y
            For y As Integer = 0 To w.yMax Step 1
                For x As Integer = 0 To w.xMax Step 1
                    If w.GetValue(x, y) Then
                        If m.symmID > -1 Then
                            Dim pp() As Point = owner.symm.ApplySymm(New Point(pos.X + x, pos.Y + y), settMap.nRaces, m, 1)
                            For k As Integer = 0 To UBound(pp) Step 1
                                Dim tx As Integer = pp(k).X
                                Dim ty As Integer = pp(k).Y
                                result(tx, ty) = True
                            Next k
                        Else
                            result(pos.X + x, pos.Y + y) = True
                        End If
                    End If
                Next x
            Next y
            Return result
        End Function

        Public Sub SetWaterCellSymm(ByRef x As Integer, ByRef y As Integer, ByRef freeCell(,) As Boolean, ByRef WaterAmount As Integer)
            If m.symmID > -1 Then
                Dim changeWaterAmount As Boolean = True
                Dim pp() As Point = owner.symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
                For k As Integer = 0 To UBound(pp) Step 1
                    Dim tx As Integer = pp(k).X
                    Dim ty As Integer = pp(k).Y
                    Call SetWaterCell(tx, ty, freeCell, WaterAmount, changeWaterAmount)
                    changeWaterAmount = False
                Next k
            Else
                Call SetWaterCell(x, y, freeCell, WaterAmount, True)
            End If
        End Sub
        Private Sub SetWaterCell(ByRef x As Integer, ByRef y As Integer, ByRef freeCell(,) As Boolean, ByRef WaterAmount As Integer, ByVal changeWaterAmount As Boolean)
            If Not IsNothing(freeCell) Then freeCell(x, y) = False
            If Not m.board(x, y).surface.isWater Then
                m.board(x, y).surface.isWater = True
                If changeWaterAmount Then WaterAmount -= 1
            End If
        End Sub
        Public Sub SetGroundCellSymm(ByRef x As Integer, ByRef y As Integer, ByRef WaterAmount As Integer)
            If m.symmID > -1 Then
                Dim changeWaterAmount As Boolean = True
                Dim pp() As Point = owner.symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
                For k As Integer = 0 To UBound(pp) Step 1
                    Dim tx As Integer = pp(k).X
                    Dim ty As Integer = pp(k).Y
                    Call SetGroundCell(tx, ty, WaterAmount, changeWaterAmount)
                    changeWaterAmount = False
                Next k
            Else
                Call SetGroundCell(x, y, WaterAmount, True)
            End If
        End Sub
        Private Sub SetGroundCell(ByRef x As Integer, ByRef y As Integer, ByRef WaterAmount As Integer, ByVal changeWaterAmount As Boolean)
            If m.board(x, y).surface.isWater Then
                m.board(x, y).surface.isWater = False
                If changeWaterAmount Then WaterAmount += 1
            End If
        End Sub

        Public Sub MakeRuinsWatered(ByRef loc As Location, ByRef WaterAmount As Integer)
            Dim chance As Double = 0.1 + settMap.WaterAmount * 0.4
            Dim makeWatered As Boolean
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    If m.board(i, j).locID(0) = loc.ID And m.board(i, j).mapObject.objectID = DefMapObjects.Types.Ruins Then
                        makeWatered = (owner.rndgen.PRand(0, 1) < chance)

                        Dim d As Integer = 1
                        Dim x1 As Integer = Math.Max(i - d, 0)
                        Dim x2 As Integer = Math.Min(i + owner.imp.ActiveObjects(m.board(i, j).mapObject.objectID).Size + d - 1, m.xSize)
                        Dim y1 As Integer = Math.Max(j - d, 0)
                        Dim y2 As Integer = Math.Min(j + owner.imp.ActiveObjects(m.board(i, j).mapObject.objectID).Size + d - 1, m.ySize)
                        For x As Integer = x1 To x2 Step 1
                            For y As Integer = y1 To y2 Step 1
                                If makeWatered Then
                                    Call SetWaterCellSymm(x, y, Nothing, WaterAmount)
                                Else
                                    Call SetGroundCellSymm(x, y, WaterAmount)
                                End If
                            Next y
                        Next x

                        x1 = Math.Max(x1 - 1, 0)
                        x2 = Math.Min(x2 + 1, m.xSize)
                        y1 = Math.Max(y1 - 1, 0)
                        y2 = Math.Min(y2 + 1, m.ySize)
                        For x As Integer = x1 To x2 Step 1
                            For y As Integer = y1 To y2 Step 1
                                If (x = x1 Or x = x2 Or y = y1 Or y = y2) And Not ((x = x1 Or x = x2) And (y = y1 Or y = y2)) And Not m.board(x, y).passability.isAttended Then
                                    If owner.rndgen.PRand(0, 1) < 0.45 Then
                                        If makeWatered Then
                                            Call SetWaterCellSymm(x, y, Nothing, WaterAmount)
                                        Else
                                            Call SetGroundCellSymm(x, y, WaterAmount)
                                        End If
                                    End If
                                End If
                            Next y
                        Next x
                    End If
                Next j
            Next i
        End Sub

    End Class
    Public Class WaterPlacer_1
        Public WaterAmountSum As Integer = 0
        Public owner As WaterGen
        Public m As Map
        Public settMap As Map.SettingsMap

        Public Sub New(ByRef w As WaterGen, ByRef myMap As Map, ByRef sM As Map.SettingsMap)
            owner = w
            m = myMap
            settMap = sM
        End Sub

        Public Sub PlaceWater(ByRef loc As Location)
            Dim lake As Location
            Dim WaterLocSettings As New Map.SettingsLoc With {.AverageRadius = CInt(0.3 * Math.Sqrt(loc.gASize * loc.gBSize) * settMap.WaterAmount), _
                                                              .maxRadiusDispersion = 0.25, _
                                                              .maxEccentricityDispersion = 0.35}
            Dim freeCell(,) As Boolean = Nothing
            Dim WaterAmount As Integer = owner.wpCommon.WaterAmountCalc(loc, freeCell)
            Dim fpInfo As New FreePointsInfo(freeCell)

            Do While WaterAmount > owner.minLocationRadiusAtAll * owner.minLocationRadiusAtAll
                lake = Location.GenLocSize(WaterLocSettings, 0, owner.rndgen, owner.minLocationRadiusAtAll)
                Dim selected As Integer = SelectLakePlace(fpInfo, 0.5 * WaterLocSettings.AverageRadius)
                If selected < 0 Then Exit Do
                Call PlaceLake(fpInfo, selected, lake, WaterAmount)
            Loop

            Call owner.wpCommon.MakeRuinsWatered(loc, WaterAmount)
            WaterAmountSum += WaterAmount
        End Sub
        Private Function SelectLakePlace(ByRef fpInfo As FreePointsInfo, ByRef lakeDist As Double) As Integer
            Dim IDs As New List(Of Integer)
            Dim add As Boolean
            Dim b As Location.Borders
            For p As Integer = 0 To 1 Step 1
                For Each i As Integer In fpInfo.IDs
                    add = True
                    b = ImpenetrableMeshGen.NearestXY(fpInfo.points(i), m, CInt(lakeDist))
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            If m.board(x, y).surface.isWater AndAlso (p = 1 OrElse fpInfo.points(i).SqDist(x, y) < lakeDist * lakeDist) Then
                                add = False
                                x = b.maxX
                                y = b.maxY
                            End If
                        Next y
                    Next x
                    If add Then IDs.Add(i)
                Next i
                If IDs.Count > 0 Then Exit For
            Next p
            If IDs.Count = 0 Then Return -1
            Return owner.comm.RandomSelection(IDs, True)
        End Function
        Private Sub PlaceLake(ByRef fpInfo As FreePointsInfo, ByRef selected As Integer, ByRef lake As Location, ByRef WaterAmount As Integer)
            'размещаем эллиптическое озеро
            fpInfo.IDs.Remove(selected)
            lake.pos = fpInfo.points(selected)
            Dim waterTiles, coastalTile, internalLakeTile As New List(Of Point)
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    If Not owner.wpCommon.HaveToBeGround(i, j) AndAlso lake.IsInside(i, j) Then
                        waterTiles.Add(New Point(i, j))
                        Call owner.wpCommon.SetWaterCellSymm(i, j, fpInfo.isFree, WaterAmount)
                    End If
                Next j
            Next i

            'делаем берег неровным
            For L As Integer = 0 To 1 Step 1
                For Each p As Point In waterTiles
                    If m.board(p.X, p.Y).surface.isWater Then
                        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(p, m, 1)
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                If Not m.board(x, y).surface.isWater Then
                                    coastalTile.Add(p)
                                    x = b.maxX
                                    y = b.maxY
                                End If
                            Next y
                        Next x
                    End If
                Next p
                For Each p As Point In coastalTile
                    If owner.rndgen.PRand(0, 1) < 0.25 - 0.1 * CDbl(L) Then
                        Call owner.wpCommon.SetGroundCellSymm(p.X, p.Y, WaterAmount)
                    End If
                Next p
                coastalTile.Clear()
            Next L

            'размещем острова
            Dim minIslandR As Integer = 2
            Dim maxIslandR As Integer = 3
            Dim threshold As Double = 1 - (1 - settMap.WaterAmount) / ((0.5 * (minIslandR + maxIslandR)) ^ 2)
            Dim dD As Double = 0.05
            For Each p As Point In waterTiles
                If m.board(p.X, p.Y).surface.isWater Then
                    Dim maybe As Boolean = True
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(p, m, 1)
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            If Not m.board(x, y).surface.isWater Then
                                maybe = False
                                x = b.maxX
                                y = b.maxY
                            End If
                        Next y
                    Next x
                    If maybe AndAlso owner.rndgen.PRand(0, 1) > threshold Then
                        Dim R As Double = owner.rndgen.PRand(CDbl(minIslandR), CDbl(maxIslandR))
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                Dim d As Double = p.Dist(x, y)
                                If m.board(x, y).surface.isWater AndAlso d < R Then
                                    If owner.rndgen.PRand(0, R + 2 * dD) > d + dD Then
                                        Call owner.wpCommon.SetGroundCellSymm(x, y, WaterAmount)
                                    End If
                                End If
                            Next y
                        Next x
                    End If
                End If
            Next p

        End Sub

        Public Sub Extend()
            Dim n As Integer
            Dim var As New List(Of Point)
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    If m.board(i, j).surface.isWater Then
                        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                        n = 0
                        var.Clear()
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                If x = i Or y = j Then
                                    If m.board(x, y).surface.isWater Then
                                        n += 1
                                    Else
                                        If (Not x = i Or Not y = j) And Not owner.wpCommon.HaveToBeGround(x, y) Then var.Add(New Point(x, y))
                                    End If
                                End If
                            Next y
                        Next x
                        If n < 2 And var.Count > 0 Then
                            n = owner.rndgen.RndPos(var.Count, True) - 1
                            Dim p As Point = var.Item(n)
                            Call owner.wpCommon.SetWaterCellSymm(p.X, p.Y, Nothing, WaterAmountSum)
                        End If
                    End If
                Next j
            Next i
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    If m.board(i, j).surface.isWater Then
                        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                        n = 0
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                If x = i Or y = j Then
                                    If m.board(x, y).surface.isWater Then n += 1
                                End If
                            Next y
                        Next x
                        If n < 2 Then Call owner.wpCommon.SetGroundCellSymm(i, j, WaterAmountSum)
                    End If
                Next j
            Next i
        End Sub

    End Class
    Public Class WaterPlacer_2
        Public owner As WaterGen
        Public m As Map
        Public settMap As Map.SettingsMap

        Private wBlocks, objWBlocks, unacceptWBlocks As Dictionary(Of String, WaterBlock)

        Public Class WaterBlock
            ''' <summary>
            ''' Блок в текстовом виде
            ''' </summary>
            Public ReadOnly key As String
            ''' <summary>
            ''' Должен ли быть тайл водным.
            ''' (row_index, column_index)
            ''' </summary>
            Private ReadOnly isWater(,) As Boolean
            ''' <summary>
            ''' Количество воды
            ''' </summary>
            Public ReadOnly waterAmount As Integer

            Public ReadOnly xMax, yMax As Integer

            Public Sub New(ByRef k As String, ByVal isObjBlock As Boolean)
                key = k
                isWater = StrToBool(k)
                xMax = UBound(isWater, 2)
                yMax = UBound(isWater, 1)
                waterAmount = CalculateWaterAmount(isObjBlock)
                If waterAmount = 0 Then
                    Dim txt As String = "No water in block" & vbNewLine & k
                    MsgBox(txt)
                    Throw New Exception(txt)
                End If
            End Sub

            Public Sub SetValue(ByRef x As Integer, ByRef y As Integer, ByRef v As Boolean)
                isWater(y, x) = v
            End Sub
            Public Function GetValue(ByRef x As Integer, ByRef y As Integer) As Boolean
                Return isWater(y, x)
            End Function

            Private Function CalculateWaterAmount(ByVal isObjBlock As Boolean) As Integer
                Dim w As Integer
                If isObjBlock Then
                    For Each u As Integer In {xMax, yMax}
                        If Not (u Mod 2) = 0 Then
                            Dim txt As String = "Unexpected block dimension" & vbNewLine & key
                            MsgBox(txt)
                            Throw New Exception(txt)
                        End If
                    Next u
                    Dim c1 As Integer = CInt(xMax / 2)
                    Dim c2 As Integer = CInt(yMax / 2)

                    For i As Integer = 0 To xMax Step 1
                        For j As Integer = 0 To yMax Step 1
                            If Not (i >= c1 - 1 And i <= c1 + 1 And j >= c2 - 1 And j <= c2 + 1) And GetValue(i, j) Then
                                w += 1
                            End If
                        Next j
                    Next i
                Else
                    For Each v As Boolean In isWater
                        If v Then w += 1
                    Next v
                End If
                Return w
            End Function

            Public Shared Function ApplySymmetry(ByRef v(,) As Boolean) As Boolean()(,)
                Dim out(4 * 2 * 2 - 1)(,) As Boolean
                out(0) = CType(v.Clone, Boolean(,))
                Dim n As Integer = 0
                For i As Integer = 1 To 3 Step 1
                    n += 1
                    out(n) = Rotate90(out(n - 1))
                Next i
                Dim m As Integer
                For p As Integer = 0 To 1 Step 1
                    m = n
                    For i As Integer = 0 To m Step 1
                        n += 1
                        out(n) = Reflection(out(i), p = 0)
                    Next i
                Next p
                Return out
            End Function
            Private Shared Function Rotate90(ByRef v(,) As Boolean) As Boolean(,)
                Dim u1 As Integer = UBound(v, 1)
                Dim u2 As Integer = UBound(v, 2)
                Dim r(u2, u1) As Boolean
                For i As Integer = 0 To u1 Step 1
                    For j As Integer = 0 To u2 Step 1
                        r(j, u1 - i) = v(i, j)
                    Next j
                Next i
                Return r
            End Function
            Private Shared Function Reflection(ByRef v(,) As Boolean, ByRef horisontal As Boolean) As Boolean(,)
                Dim u1 As Integer = UBound(v, 1)
                Dim u2 As Integer = UBound(v, 2)
                Dim r(u1, u2) As Boolean
                If horisontal Then
                    For i As Integer = 0 To u1 Step 1
                        For j As Integer = 0 To u2 Step 1
                            r(i, u2 - j) = v(i, j)
                        Next j
                    Next i
                Else
                    For i As Integer = 0 To u1 Step 1
                        For j As Integer = 0 To u2 Step 1
                            r(u1 - i, j) = v(i, j)
                        Next j
                    Next i
                End If
                Return r
            End Function

            Public Shared Function StrToBool(ByRef blockKey As String) As Boolean(,)
                Dim lines() As String = blockKey.Split(Chr(10))
                Dim v(UBound(lines), lines(0).Length - 1) As Boolean
                For i As Integer = 0 To UBound(lines) Step 1
                    If Not lines(i).Length = lines(0).Length Then
                        Dim txt As String = "Unexpected length of line " & i + 1 & " in block" & vbNewLine & blockKey
                        MsgBox(txt)
                        Throw New Exception(txt)
                    End If
                    For j As Integer = 0 To lines(i).Length - 1 Step 1
                        v(i, j) = StrToBool(lines(i).Substring(j, 1), blockKey)
                    Next j
                Next i
                Return v
            End Function
            Public Shared Function StrToBool(ByRef v As String, ByRef blockKey As String) As Boolean
                If v.ToUpper = "W" Then
                    Return True
                ElseIf v.ToUpper = "G" Then
                    Return False
                Else
                    Dim txt As String = "Unexpected symbol " & v & " in block" & vbNewLine & blockKey
                    MsgBox(txt)
                    Throw New Exception(txt)
                End If
            End Function
            Public Shared Function BoolToStr(ByRef v(,) As Boolean) As String
                Dim k As String = ""
                For i As Integer = 0 To UBound(v, 1) Step 1
                    If i > 0 Then k &= Chr(10)
                    For j As Integer = 0 To UBound(v, 2) Step 1
                        k &= BoolToStr(v(i, j))
                    Next j
                Next i
                Return k
            End Function
            Public Shared Function BoolToStr(ByRef v As Boolean) As String
                If v Then
                    Return "W"
                Else
                    Return "G"
                End If
            End Function

        End Class

        Public Sub New(ByRef w As WaterGen, ByRef myMap As Map, ByRef sM As Map.SettingsMap)
            owner = w
            m = myMap
            settMap = sM
            wBlocks = ReadWaterBlocks(owner.comm.defValues.WaterBlocksCommon, False)
            objWBlocks = ReadWaterBlocks(owner.comm.defValues.WaterBlocks3x3Objects, True)
            unacceptWBlocks = ReadWaterBlocks(owner.comm.defValues.WaterBlocksUnacceptable, False)
        End Sub
        Private Function ReadWaterBlocks(ByVal blocksText As String, ByVal isObjBlock As Boolean) As Dictionary(Of String, WaterBlock)
            Dim result As New Dictionary(Of String, WaterBlock)
            Dim lines() As String = owner.comm.TxtSplit(blocksText)
            Dim key As String = ""
            For i As Integer = 0 To UBound(lines) Step 1
                If lines(i).StartsWith("_") Then
                    Call AddNewBlock(key, result, isObjBlock)
                    key = ""
                Else
                    If Not key = "" Then key &= Chr(10)
                    key &= lines(i).ToUpper
                End If
            Next i
            Call AddNewBlock(key, result, isObjBlock)
            Return result
        End Function
        Private Sub AddNewBlock(ByRef key As String, ByRef dest As Dictionary(Of String, WaterBlock), ByVal isObjBlock As Boolean)
            If key = "" Then Exit Sub
            If dest.ContainsKey(key) Then Exit Sub
            Dim k As String
            Dim s()(,) As Boolean = WaterBlock.ApplySymmetry(WaterBlock.StrToBool(key))
            For i As Integer = 0 To UBound(s) Step 1
                k = WaterBlock.BoolToStr(s(i))
                If Not dest.ContainsKey(k) Then dest.Add(k, New WaterBlock(k, isObjBlock))
            Next i
        End Sub

        Public Sub PlaceWater(ByRef loc As Location)
            'определяем, сколько может быть воды в локации, затем если можем, с каким-то шансом добавляем воду под торговцев/руины
            'если еще можем разместить воду, то добавляем блоки
            Dim freeCell(,) As Boolean = Nothing
            Dim WaterAmount As Integer = owner.wpCommon.WaterAmountCalc(loc, freeCell, 3)
            Dim fpInfo As New FreePointsInfo(freeCell)

            Call AddWaterToAttendedObjects(loc, fpInfo, WaterAmount)
            Call AddWaterCommon(loc, fpInfo, WaterAmount)
        End Sub

        Private Sub AddWaterToAttendedObjects(ByVal loc As Location, ByVal fpInfo As FreePointsInfo, ByRef WaterAmount As Integer)
            Dim chance As Double = 0.1 + settMap.WaterAmount * 0.9
            Dim pList As New List(Of Point)
            For j As Integer = 0 To m.ySize Step 1
                For i As Integer = 0 To m.xSize Step 1
                    If m.board(i, j).locID(0) = loc.ID _
                    AndAlso owner.waterAttendedObj.ContainsKey(m.board(i, j).mapObject.objectID) Then
                        'не проверяем расы, т.к. они определяются где-то после добавления воды
                        'AndAlso ContainsAny(owner.waterAttendedObj.Item(m.board(i, j).mapObject.objectID), m.board(i, j).mapObject.objRace) Then
                        If owner.rndgen.RndDblFast(0, 1) <= chance Then pList.Add(New Point(i, j))
                    End If
                Next i
            Next j
            If pList.Count = 0 Then Exit Sub
            Dim HaveToBeGround_bak(,) As Boolean = CType(owner.wpCommon.HaveToBeGround.Clone, Boolean(,))
            Dim isFree_bak(,) As Boolean = CType(fpInfo.isFree.Clone, Boolean(,))
            Dim objID As Integer
            Dim possibleWaterPlaces As New Dictionary(Of String, Integer)

            For Each p As Point In pList
                owner.wpCommon.HaveToBeGround = CType(HaveToBeGround_bak.Clone, Boolean(,))
                fpInfo.isFree = CType(isFree_bak.Clone, Boolean(,))

                objID = m.board(p.X, p.Y).mapObject.objectID
                Dim xb1 As Integer = p.X - owner.imp.ActiveObjects(objID).dxy
                Dim xb2 As Integer = xb1 + UBound(owner.imp.ObjectBlank(objID), 1)
                Dim yb1 As Integer = p.Y - owner.imp.ActiveObjects(objID).dxy
                Dim yb2 As Integer = yb1 + UBound(owner.imp.ObjectBlank(objID), 2)
                For y As Integer = yb1 To yb2 Step 1
                    For x As Integer = xb1 To xb2 Step 1
                        If owner.imp.ObjectBlank(objID)(x - xb1, y - yb1).passability.isAttended _
                        Or owner.imp.ObjectBlank(objID)(x - xb1, y - yb1).passability.isPenetrable Then
                            owner.wpCommon.HaveToBeGround(x, y) = False
                            fpInfo.isFree(x, y) = True
                        End If
                    Next x
                Next y

                possibleWaterPlaces.Clear()
                Dim keys() As String = objWBlocks.Keys.ToArray
                Dim addsWaterAmount(UBound(keys)) As Integer
                Dim pWaterAmount As Integer = WaterAmount
                Dim pp As Point = p
                Parallel.For(0, keys.Length, _
                 Sub(i As Integer)
                     Dim pos As Point = ObjectWaterPos(pp, objID, GetWaterBlock(keys(i), True))
                     addsWaterAmount(i) = CanAddWatherAmount(loc, fpInfo, pWaterAmount, pos, keys(i), True, True)
                 End Sub)

                For i As Integer = 0 To UBound(keys) Step 1
                    If addsWaterAmount(i) > 0 Then
                        Dim pos As Point = ObjectWaterPos(p, objID, GetWaterBlock(keys(i), True))
                        possibleWaterPlaces.Add(keys(i) & "_" & pos.X & "_" & pos.Y, addsWaterAmount(i))
                    End If
                Next i
                If possibleWaterPlaces.Count > 0 Then
                    Dim idsArray() As String = possibleWaterPlaces.Keys.ToArray
                    Dim ids As List(Of Integer) = WaterGen.FreePointsInfo.MakeIDsList(0, UBound(idsArray))
                    Dim weight() As Double = MakeWeight(possibleWaterPlaces, idsArray, WaterAmount)
                    Dim selected As Integer = owner.comm.RandomSelection(ids, weight, True, True)
                    Dim k() As String = idsArray(selected).Split(CChar("_"))
                    Dim key As String = k(0)
                    Dim x As Integer = CInt(k(1))
                    Dim y As Integer = CInt(k(2))
                    Call AddWater(WaterAmount, key, x, y, True)
                End If
            Next p
            owner.wpCommon.HaveToBeGround = CType(HaveToBeGround_bak.Clone, Boolean(,))
            fpInfo.isFree = CType(isFree_bak.Clone, Boolean(,))
        End Sub
        Private Function ObjectWaterPos(ByRef p As Point, ByRef objID As Integer, ByRef w As WaterBlock) As Point
            Return New Point(p.X - CInt((w.xMax + 1 - owner.imp.ActiveObjects(objID).Size) / 2), _
                             p.Y - CInt((w.yMax + 1 - owner.imp.ActiveObjects(objID).Size) / 2))
        End Function
        Private Function ContainsAny(ByRef L1 As List(Of Integer), ByRef L2 As List(Of Integer)) As Boolean
            If L1.Count >= L2.Count Then
                For Each i As Integer In L2
                    If L1.Contains(i) Then Return True
                Next i
            Else
                For Each i As Integer In L1
                    If L2.Contains(i) Then Return True
                Next i
            End If
            Return False
        End Function

        Private Sub AddWaterCommon(ByVal loc As Location, ByVal fpInfo As FreePointsInfo, ByRef WaterAmount As Integer)
            Dim possibleWaterPlaces As New Dictionary(Of String, Integer)
            possibleWaterPlaces.Add("---", 0)
            Dim addsWaterAmount(UBound(fpInfo.points)), pWaterAmount As Integer
            Dim pKey As String
            Do While WaterAmount > 0 And possibleWaterPlaces.Count > 0
                possibleWaterPlaces.Clear()
                For Each k As String In wBlocks.Keys
                    pKey = k
                    pWaterAmount = WaterAmount
                    Parallel.For(0, fpInfo.points.Length, _
                     Sub(i As Integer)
                         addsWaterAmount(i) = CanAddWatherAmount(loc, fpInfo, pWaterAmount, fpInfo.points(i), pKey, False)
                     End Sub)
                    For i As Integer = 0 To UBound(fpInfo.points) Step 1
                        If addsWaterAmount(i) > 0 Then
                            possibleWaterPlaces.Add(k & "_" & fpInfo.points(i).X & "_" & fpInfo.points(i).Y, addsWaterAmount(i))
                        End If
                    Next i
                Next k
                If possibleWaterPlaces.Count > 0 Then
                    Dim idsArray() As String = possibleWaterPlaces.Keys.ToArray
                    Dim ids As List(Of Integer) = WaterGen.FreePointsInfo.MakeIDsList(0, UBound(idsArray))
                    Dim weight() As Double = MakeWeight(possibleWaterPlaces, idsArray, WaterAmount)
                    Dim selected As Integer = owner.comm.RandomSelection(ids, weight, True, True)
                    Dim k() As String = idsArray(selected).Split(CChar("_"))
                    Dim key As String = k(0)
                    Dim x As Integer = CInt(k(1))
                    Dim y As Integer = CInt(k(2))
                    Call AddWater(WaterAmount, key, x, y, False)
                End If
            Loop
        End Sub
        Private Function MakeWeight(ByRef possibleWaterPlaces As Dictionary(Of String, Integer), ByRef idsArray() As String, ByRef WaterAmount As Integer) As Double()
            Dim weight(UBound(idsArray)) As Double
            Dim w As Integer
            For i As Integer = 0 To UBound(idsArray) Step 1
                w = possibleWaterPlaces.Item(idsArray(i))
                weight(i) = w ^ 2
                If w > WaterAmount Then weight(i) /= (w - WaterAmount)
            Next i
            Return weight
        End Function
        Private Function GetWaterBlock(ByRef key As String, ByVal useObjWBlocks As Boolean) As WaterBlock
            If useObjWBlocks Then
                Return objWBlocks.Item(key)
            Else
                Return wBlocks.Item(key)
            End If
        End Function
        Private Function CanAddWatherAmount(ByRef loc As Location, ByRef fpInfo As FreePointsInfo, ByRef WaterAmount As Integer, _
                                            ByRef p As Point, ByRef key As String, ByVal useObjWBlocks As Boolean, _
                                            Optional ByVal IgnoreSize As Boolean = False) As Integer
            Dim w As WaterBlock = GetWaterBlock(key, useObjWBlocks)

            Dim x2 As Integer = p.X + w.xMax
            Dim y2 As Integer = p.Y + w.yMax
            If Not IgnoreSize Then
                If p.X < 0 Then Return 0
                If p.Y < 0 Then Return 0
                If x2 > fpInfo.xMax Then Return 0
                If y2 > fpInfo.yMax Then Return 0
            End If

            Dim hasLocID As Boolean = False
            Dim hasGroundTileToWater As Boolean = False
            Dim addsWaterAmount As Integer = 0
            For j As Integer = p.Y To y2 Step 1
                If Not IgnoreSize Or YFilter(j) Then
                    For i As Integer = p.X To x2 Step 1
                        If Not IgnoreSize Or XFilter(i) Then
                            If w.GetValue(i - p.X, j - p.Y) Then
                                If owner.wpCommon.HaveToBeGround(i, j) Then Return 0
                                If Not m.board(i, j).passability.isBorder And Not fpInfo.isFree(i, j) Then Return 0
                                If m.board(i, j).locID(0) = loc.ID Then hasLocID = True
                                If Not m.board(i, j).surface.isWater And fpInfo.isFree(i, j) Then
                                    hasGroundTileToWater = True
                                    addsWaterAmount += 1
                                End If
                            End If
                        End If
                    Next i
                End If
            Next j
            If Not hasLocID Then Return 0
            If Not hasGroundTileToWater Then Return 0
            If WaterAmount + 2 < addsWaterAmount Then Return 0
            If addsWaterAmount > 0 Then
                Dim wTest(,) As Boolean = owner.wpCommon.MakeWaterSufaceTestArray(p, w)
                For Each u As WaterBlock In unacceptWBlocks.Values
                    If TestByUnacceptableBlock(wTest, u, w, p) Then Return 0
                Next u
            End If
            Return addsWaterAmount
        End Function
        Private Function XFilter(ByRef x As Integer) As Boolean
            Return (x >= 0 And x <= m.xSize)
        End Function
        Private Function YFilter(ByRef y As Integer) As Boolean
            Return (y >= 0 And y <= m.ySize)
        End Function
        Private Function TestByUnacceptableBlock(ByRef wTest(,) As Boolean, ByRef unacceptable As WaterBlock, _
                                                 ByRef adding As WaterBlock, ByRef p As Point) As Boolean
            Dim X1 As Integer = Math.Max(0, p.X - unacceptable.xMax)
            Dim X2 As Integer = Math.Min(m.xSize - unacceptable.xMax, p.X + adding.xMax)
            Dim Y1 As Integer = Math.Max(0, p.Y - unacceptable.yMax)
            Dim Y2 As Integer = Math.Min(m.ySize - unacceptable.yMax, p.Y + adding.yMax)
            Dim runNextTestLoop As Boolean
            For y As Integer = Y1 To Y2 Step 1
                For x As Integer = X1 To X2 Step 1
                    runNextTestLoop = False
                    For j As Integer = 0 To unacceptable.yMax Step 1
                        For i As Integer = 0 To unacceptable.xMax Step 1
                            If Not wTest(x + i, y + j) = unacceptable.GetValue(i, j) Then
                                runNextTestLoop = True
                                Exit For
                            End If
                        Next i
                        If runNextTestLoop Then Exit For
                    Next j
                    If Not runNextTestLoop Then Return True
                Next x
            Next y
            Return False
        End Function
        Private Sub AddWater(ByRef WaterAmount As Integer, ByRef key As String, ByRef x As Integer, ByRef y As Integer, ByVal useObjWBlocks As Boolean)
            Dim w As WaterBlock = GetWaterBlock(key, useObjWBlocks)
            Dim x2 As Integer = x + w.xMax
            Dim y2 As Integer = y + w.yMax
            For j As Integer = y To y2 Step 1
                If YFilter(j) Then
                    For i As Integer = x To x2 Step 1
                        If XFilter(i) Then
                            If w.GetValue(i - x, j - y) Then
                                Call owner.wpCommon.SetWaterCellSymm(i, j, Nothing, WaterAmount)
                            End If
                        End If
                    Next i
                End If
            Next j
        End Sub
    End Class

End Class

Public Class ImpenetrableObjects

    Class MapObject
        Inherits DecorationPlacingPropertiesFields
        ''' <summary>Ширина объекта</summary>
        Protected Friend xSize As Integer
        ''' <summary>Высота объекта</summary>
        Protected Friend ySize As Integer
        ''' <summary>Имя объекта</summary>
        Protected Friend name As String
    End Class
    Class Landmark
        Inherits MapObject
        ''' <summary>Тэги для подзон</summary>
        ''' <remarks></remarks>
        Public tags As List(Of String)
    End Class
    Class PlateauObject
        Inherits MapObject
        ''' <summary>Как скреплять объекты</summary>
        Protected Friend connectors()() As Point
        ''' <summary>Эти точки должны быть на границе карты</summary>
        Protected Friend border As Point
        ''' <summary>Является ли водопадом</summary>
        Protected Friend isWaterfall As Boolean
    End Class

    Private symm As New SymmetryOperations
    Private comm As Common
    Private objects As Landmark()
    Private mountains, ruins, mages, merchants, mercenaries, trainers As MapObject()
    Private plateau() As PlateauObject
    Private maxPlateauSize As Integer
    Private maxChainLen As Integer = 7
    Private raceSpells() As AllDataStructues.Spell
    Private raceIdToString As New Dictionary(Of Integer, String)
    Private constructorMsg As String = ""
    Private racesSublocations(-1) As Dictionary(Of String, sublocationProperties)

    Class sublocationProperties
        Public objectsID() As Integer
        Public tag As String
        Public race As String
        Public raceID As Integer
        Public radius As Double

        Public sizeAmount(-1) As Integer

        Public Sub New(ByRef allObjects() As Landmark, ByRef lRace As String, ByRef lTag As String, _
                       ByRef lRadius As Double, ByRef c As Common)
            tag = lTag.ToUpper
            race = lRace.ToUpper
            raceID = c.RaceIdentifierToSubrace(race)
            radius = lRadius
            Dim n As Integer = -1
            ReDim objectsID(UBound(allObjects))
            For i As Integer = 0 To UBound(allObjects) Step 1
                If allObjects(i).race.Contains(raceID) AndAlso allObjects(i).tags.Contains(tag) Then
                    n += 1
                    objectsID(n) = i
                    If UBound(sizeAmount) < allObjects(i).xSize Then ReDim Preserve sizeAmount(allObjects(i).xSize)
                    sizeAmount(allObjects(i).xSize) += 1
                End If
            Next i
            ReDim Preserve objectsID(n)
            If sizeAmount(1) = 0 Then Throw New Exception("There are no objects with size of 1 for race " & race & " and tag " & tag)
        End Sub

    End Class

    Private Structure PlateauPlacingResult
        Dim obj() As PlateauObject
        Dim pos() As Point
        Dim n As Integer
    End Structure

    Public Structure GlobalMapDecoration
        ''' <summary>GxxxMGxxxx</summary>
        Dim ID As String
        ''' <summary>Размеры объекта</summary>
        Dim Size As Size

        ''' <summary>Вернет размер горы</summary>
        ''' <param name="mountainID">MOMNExxyy</param>
        Public Shared Function MountainSize(ByRef mountainID As String) As Size
            Dim s As Integer = CInt(mountainID.Substring(GenDefaultValues.wObjKeyMountain.Length, 2))
            Return New Size(s, s)
        End Function

        Public Shared Function GetObjectSize(ByRef allLandmarks() As GlobalMapDecoration, ByRef landmarkID As String) As Size
            If landmarkID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMountain.ToUpper) Then
                Return MountainSize(landmarkID)
            Else
                Dim id As String = landmarkID.ToUpper
                For i As Integer = 0 To UBound(allLandmarks) Step 1
                    If allLandmarks(i).ID.ToUpper = id Then Return New Size(allLandmarks(i).Size.Width, allLandmarks(i).Size.Height)
                Next i
            End If
        End Function
    End Structure

    ''' <param name="ObjectsSize">Размеры всех объектов местности</param>
    ''' <param name="AddLoreUnitsToExcluded">Добавлять ли лорных юнитов в список исключенных</param>
    ''' <param name="spells">Все заклинания в игре</param>
    Public Sub New(ByRef ObjectsSize() As GlobalMapDecoration, ByRef AddLoreUnitsToExcluded As Boolean, ByRef spells() As AllDataStructues.Spell, ByRef c As Common)
        comm = c
        If AddLoreUnitsToExcluded Then Call comm.ReadExcludedLoreObjectsList()
        Call comm.ReadExcludedObjectsList()
        Call comm.ReadCustomBuildingRace()
        Call comm.ReadPlateauConstructionDescription()

        raceSpells = spells

        Dim objType() As String = New String() {My.Resources.objKeyMage, My.Resources.objKeyMercenaries, My.Resources.objKeyMerchant, _
                                                My.Resources.objKeyMountain, My.Resources.objKeyRuin, My.Resources.objKeyTrainer, ""}
        Dim objList(UBound(objType))() As MapObject
        For Each k As GlobalMapDecoration In ObjectsSize
            If Not comm.excludedObjects.Contains(k.ID.ToUpper) AndAlso comm.objectRace.ContainsKey(k.ID.ToUpper) Then
                If comm.objectRace.Item(k.ID.ToUpper).race.Count > 0 _
                And (comm.objectRace.Item(k.ID.ToUpper).ground Or comm.objectRace.Item(k.ID.ToUpper).water) Then

                    Dim g As MapObject = New MapObject With {.xSize = k.Size.Width, _
                                                             .ySize = k.Size.Height, _
                                                             .ground = comm.objectRace.Item(k.ID.ToUpper).ground, _
                                                             .water = comm.objectRace.Item(k.ID.ToUpper).water, _
                                                             .race = comm.objectRace.Item(k.ID.ToUpper).race, _
                                                             .name = k.ID.ToUpper}
                    If comm.PlateauConstruction.ContainsKey(k.ID.ToUpper) Then
                        Dim isWaterfall As Boolean = False
                        Dim connectors()() As Point = Nothing
                        Dim boundary As Point = Nothing
                        Dim t() As String = comm.PlateauConstruction.Item(k.ID.ToUpper).ToUpper.Replace(vbTab, " ").Split(CChar(" "))
                        Dim dx As Integer = 3 - g.xSize
                        Dim dy As Integer = 3 - g.ySize
                        For i As Integer = 0 To UBound(t) Step 1
                            If t(i) = "W" Then
                                isWaterfall = True
                            ElseIf t(i) = "U" Or t(i) = "D" Or t(i) = "L" Or t(i) = "R" Then
                                Dim p1 As Point = Nothing
                                Dim p2 As Point = Nothing
                                If IsNothing(connectors) Then
                                    ReDim connectors(0)
                                Else
                                    ReDim Preserve connectors(connectors.Length)
                                End If
                                If t(i) = "U" Then
                                    p1 = New Point(1, -1)
                                    p2 = New Point(1, 0)
                                ElseIf t(i) = "D" Then
                                    p1 = New Point(1, 2 - dy)
                                    p2 = New Point(1, 3 - dy)
                                ElseIf t(i) = "L" Then
                                    p1 = New Point(-1, 1)
                                    p2 = New Point(0, 1)
                                ElseIf t(i) = "R" Then
                                    p1 = New Point(2 - dx, 1)
                                    p2 = New Point(3 - dx, 1)
                                End If
                                connectors(UBound(connectors)) = New Point() {p1, p2}
                            ElseIf t(i) = "UB" Or t(i) = "DB" Or t(i) = "LB" Or t(i) = "RB" Then
                                Dim p1 As Point = Nothing
                                If t(i) = "UB" Then
                                    p1 = New Point(1, 0)
                                ElseIf t(i) = "DB" Then
                                    p1 = New Point(1, 2 - dy)
                                ElseIf t(i) = "LB" Then
                                    p1 = New Point(0, 1)
                                ElseIf t(i) = "RB" Then
                                    p1 = New Point(2 - dx, 1)
                                End If
                                boundary = p1
                            End If
                        Next i
                        If IsNothing(plateau) Then
                            ReDim plateau(0)
                        Else
                            ReDim Preserve plateau(plateau.Length)
                        End If
                        plateau(UBound(plateau)) = New PlateauObject With {.xSize = g.xSize, _
                                                                           .ySize = g.ySize, _
                                                                           .ground = g.ground, _
                                                                           .water = g.water, _
                                                                           .race = g.race, _
                                                                           .name = g.name, _
                                                                           .isWaterfall = isWaterfall, _
                                                                           .connectors = connectors, _
                                                                           .border = boundary}
                        maxPlateauSize = Math.Max(maxPlateauSize, Math.Max(g.xSize, g.ySize))
                    Else
                        For i As Integer = 0 To UBound(objType) Step 1
                            If i = UBound(objType) Then
                                If IsNothing(objects) Then
                                    ReDim objects(0)
                                Else
                                    ReDim Preserve objects(objects.Length)
                                End If
                                objects(UBound(objects)) = New Landmark With {.ground = g.ground, _
                                                                              .name = g.name, _
                                                                              .race = g.race, _
                                                                              .water = g.water, _
                                                                              .xSize = g.xSize, _
                                                                              .ySize = g.ySize, _
                                                                              .tags = comm.objectRace.Item(k.ID.ToUpper).tags}
                            ElseIf k.ID.Length > objType(i).Length AndAlso k.ID.ToUpper.Substring(0, objType(i).Length) = objType(i).ToUpper Then
                                If IsNothing(objList(i)) Then
                                    ReDim objList(i)(0)
                                Else
                                    ReDim Preserve objList(i)(objList(i).Length)
                                End If
                                objList(i)(UBound(objList(i))) = g
                                Exit For
                            End If
                        Next i
                    End If
                End If
            End If
        Next k
        mages = objList(0)
        mercenaries = objList(1)
        merchants = objList(2)
        mountains = objList(3)
        ruins = objList(4)
        trainers = objList(5)
        'objects = objList(6)

        Dim races() As String = comm.TxtSplit(comm.defValues.Races)
        For Each s As String In races
            Dim splited() As String = s.Split(CChar(" "))
            raceIdToString.Add(CInt(splited(UBound(splited))), splited(1).ToUpper)
        Next s

        Dim rSublocations() As String = comm.TxtSplit(comm.defValues.RaceSublocations)
        For Each Str As String In rSublocations
            Dim s() As String = Str.Split(CChar(" "))
            Dim r As Integer = comm.RaceIdentifierToSubrace(s(0))
            If UBound(racesSublocations) < r Then ReDim Preserve racesSublocations(r)
            For j As Integer = 1 To UBound(s) Step 1
                Dim tag As String = s(j).Split(CChar(":"))(0).ToUpper
                Dim radius As Double = CDbl(s(j).Split(CChar(":"))(1))
                If IsNothing(racesSublocations(r)) Then racesSublocations(r) = New Dictionary(Of String, sublocationProperties)
                racesSublocations(r).Add(tag, New sublocationProperties(objects, _
                                                                        comm.defValues.RaceNumberToRaceChar(r), _
                                                                        tag, radius, comm))
            Next j
        Next Str

        constructorMsg = vbNewLine & "------------------" & vbNewLine & _
                         "Landscape content:" & vbNewLine &
                         "Ruins amount: " & ruins.Length & vbNewLine & _
                         "Vendors amount: " & merchants.Length & vbNewLine & _
                         "Mages amount: " & mages.Length & vbNewLine & _
                         "Mercenaries amount: " & mercenaries.Length & vbNewLine & _
                         "Trainers amount: " & trainers.Length & vbNewLine & _
                         "Landmarks amount: " & objects.Length & vbNewLine & _
                         "Mountains amount: "
        Dim mountainSizeAmount(-1) As Integer
        For i As Integer = 0 To UBound(mountains) Step 1
            If mountains(i).xSize > UBound(mountainSizeAmount) Then ReDim Preserve mountainSizeAmount(mountains(i).xSize)
            mountainSizeAmount(mountains(i).xSize) += 1
        Next i
        For i As Integer = 0 To UBound(mountainSizeAmount) Step 1
            If mountainSizeAmount(i) > 0 Then
                constructorMsg &= i & ":" & mountainSizeAmount(i) & " | "
            End If
        Next i
        constructorMsg = constructorMsg.Remove(constructorMsg.Length - 3) & vbNewLine & _
                         "------------------" & vbNewLine
    End Sub
    ''' <summary>Определит ID объектов и настройки содержимого лавок</summary>
    ''' <param name="m">Карта со сгенерированными расами отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону</param>
    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef settCommLoc As Map.SettingsLoc)
        'If Not settRaceLoc.isChecked Then Throw New Exception("Check parameters via settRaceLoc.Check()")
        'If Not settCommLoc.isChecked Then Throw New Exception("Check parameters via settCommLoc.Check()")
        Dim a() As Map.SettingsLoc = Map.SettingsLoc.ToArray(settRaceLoc, settCommLoc, settMap.nRaces, m.Loc.Length)
        Call Gen(m, settMap, a)
    End Sub

    ''' <summary>Определит ID объектов и настройки содержимого лавок</summary>
    ''' <param name="m">Карта со сгенерированными расами отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settLoc">Настройки для каждой локации. Первыми должны идти стартовые локации рас.
    ''' Коментарий к настройкам стартовых локаций играбельных рас:
    ''' дробная часть определяет шанс округления большую сторону.
    ''' Комментарий к настройкам остальных локаций:
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади, заданной в настройках (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт </param>
    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc)

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        'For i As Integer = 0 To UBound(settLoc) Step 1
        '    If Not settLoc(i).isChecked Then Throw New Exception("Check parameters via settLoc(" & i & ").Check()")
        'Next i

        If Not m.complited.StacksRaceGen_Done Then
            Throw New Exception("Сначала нужно выполнить RaceGen.Gen")
        End If

        comm = m.comm

        Dim t0 As Integer = Environment.TickCount

        Dim free(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                free(x, y) = True
            Next x
        Next y

        Call PlaceCities(m, settMap)
        Call CorrectTownGuardsDesiredStats(m)
        Call PlaceAttendedObjects(m)
        Call PlaceMines(m, settMap, settLoc)
        Call PlacePlateau(m, free)
        Call PlaceMouintains(m, free)

        Call SetObjectsTags(m, free)
        Call PlaceOtherObjects(m, free)
        'Call AddSpells(m, settMap, settLoc)
        'Call AddMercenaries(m, settMap, settLoc)
        'Call AddMerchantItems(m, settMap, settLoc)

        m.log.Add(constructorMsg)

        Call m.log.Add("Objects types definition: " & Environment.TickCount - t0 & " ms")

        m.complited.ImpenetrableObjectsPlacing_Done = True
    End Sub

    Private Sub SetObjectsTags(ByRef m As Map, ByVal free(,) As Boolean)
        Dim tmpm As Map = m
        Dim pointID(tmpm.xSize, tmpm.ySize) As Integer
        Dim pointPos(pointID.Length - 1) As Point
        Dim n As Integer = -1
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                n += 1
                pointID(x, y) = n
                pointPos(n) = New Point(x, y)
                tmpm.board(x, y).mapObject.NewTagsList()
            Next x
        Next y
        Parallel.For(0, tmpm.Loc.Length, _
         Sub(i As Integer)
             Dim rndgen As New RndValueGen
             Dim posPool, del As New List(Of Integer)
             Dim minSublocationR As Double = Double.MaxValue
             Dim locRace As Integer = tmpm.Loc(i).Race
             For Each k As String In racesSublocations(locRace).Keys
                 minSublocationR = Math.Min(minSublocationR, racesSublocations(locRace).Item(k).radius)
             Next k
             minSublocationR *= 0.75
             For y As Integer = 0 To tmpm.ySize Step 1
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If tmpm.board(x, y).passability.isBorder And free(x, y) And tmpm.board(x, y).locID(0) = tmpm.Loc(i).ID Then
                         posPool.Add(pointID(x, y))
                     End If
                 Next x
             Next y
             While posPool.Count > 0
                 Dim selectedPointID As Integer = posPool(rndgen.RndIntFast(0, posPool.Count - 1))
                 Dim selectedTag As String = racesSublocations(locRace).Keys(rndgen.RndIntFast(0, racesSublocations(locRace).Count - 1))
                 Dim r2Set As Double = racesSublocations(locRace).Item(selectedTag).radius ^ 2
                 Dim r2Rem As Double = (racesSublocations(locRace).Item(selectedTag).radius + minSublocationR) ^ 2
                 del.Clear()
                 For Each pid As Integer In posPool
                     If pointPos(pid).SqDist(pointPos(selectedPointID)) < r2Set Then
                         tmpm.board(pointPos(selectedPointID).X, pointPos(selectedPointID).Y).mapObject.AddTag(selectedTag)
                     End If
                     If pointPos(pid).SqDist(pointPos(selectedPointID)) < r2Rem Then del.Add(pid)
                 Next pid
                 For Each pid As Integer In del
                     posPool.Remove(pid)
                 Next pid
             End While
             posPool.Clear()
             For y As Integer = 0 To tmpm.ySize Step 1
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If tmpm.board(x, y).locID(0) = tmpm.Loc(i).ID Then
                         If tmpm.board(x, y).mapObject.TagsList.Count = 0 Then posPool.Add(pointID(x, y))
                     End If
                 Next x
             Next y
             If posPool.Count > 0 Then
                 Do While posPool.Count > 0
                     Dim tags As New List(Of String)
                     Dim selectedPointID As Integer = posPool(rndgen.RndIntFast(0, posPool.Count - 1))
                     Dim x As Integer = pointPos(selectedPointID).X
                     Dim y As Integer = pointPos(selectedPointID).Y
                     tags.Clear()
                     Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                     For p As Integer = b.minY To b.maxY Step 1
                         For q As Integer = b.minX To b.maxX Step 1
                             If tmpm.board(q, p).mapObject.TagsList.Count > 0 Then
                                 Dim tL As List(Of String) = tmpm.board(q, p).mapObject.TagsList
                                 For Each t As String In tL
                                     If Not tags.Contains(t) Then tags.Add(t)
                                 Next t
                             End If
                         Next q
                     Next p
                     If tags.Count > 0 Then
                         Dim t As String = tags(rndgen.RndIntFast(0, tags.Count - 1))
                         tmpm.board(x, y).mapObject.AddTag(t)
                         posPool.Remove(selectedPointID)
                     End If
                 Loop
             End If
         End Sub)
        m = tmpm
    End Sub

    Private Function makeIDs(ByRef a() As MapObject) As List(Of Integer)
        Dim IDs As New List(Of Integer)
        For i As Integer = 0 To UBound(plateau) Step 1
            IDs.Add(i)
        Next i
        Return IDs
    End Function
    Private Function makeIDs(ByRef a() As PlateauObject, ByRef initOnly As Boolean, ByRef noWaterfalls As Boolean) As List(Of Integer)
        Dim IDs As New List(Of Integer)
        If Not IsNothing(a) Then
            If initOnly Then
                For i As Integer = 0 To UBound(a) Step 1
                    If a(i).connectors.Length = 1 Then IDs.Add(i)
                Next i
            Else
                For i As Integer = 0 To UBound(a) Step 1
                    If (noWaterfalls And Not a(i).isWaterfall) Or Not noWaterfalls Then IDs.Add(i)
                Next i
            End If
        End If
        Return IDs
    End Function

#Region "MayPlace"
    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, _
                              ByRef free(,) As Boolean, ByRef obj As MapObject) As Boolean
        Return MayPlace(m, x, y, free, obj.xSize, obj.ySize, obj.ground, obj.water, obj.race)
    End Function
    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, _
                              ByRef free(,) As Boolean, ByRef obj As Landmark) As Boolean
        Dim ok As Boolean = False
        For Each t As String In obj.tags
            If m.board(x, y).mapObject.InTagsList(t) Then
                ok = True
                Exit For
            End If
        Next t
        If Not ok Then Return False
        Return MayPlace(m, x, y, free, obj.xSize, obj.ySize, obj.ground, obj.water, obj.race)
    End Function
    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, _
                              ByRef free(,) As Boolean, ByRef obj As PlateauObject, ByRef Connector(,) As Integer) As Boolean
        If Not IsNothing(obj.border) Then
            If obj.border.X = 0 Then
                If x > 0 Then Return False
            ElseIf obj.border.Y = 0 Then
                If y > 0 Then Return False
            ElseIf obj.border.X + 1 = obj.xSize Then
                If Not x + obj.border.X = m.xSize Then Return False
            ElseIf obj.border.Y + 1 = obj.ySize Then
                If Not y + obj.border.Y = m.ySize Then Return False
            End If
        End If
        Dim xx, yy As Integer
        For i As Integer = 0 To UBound(obj.connectors) Step 1
            For j As Integer = 0 To UBound(obj.connectors(i)) Step 1
                Call connectorPos(xx, yy, x, y, obj.connectors(i)(j))
                If xx < 0 Or yy < 0 Or xx > m.xSize Or yy > m.ySize Then Return False
            Next j
        Next i
        If Not IsNothing(Connector) Then
            Dim t As Boolean
            For i As Integer = 0 To UBound(obj.connectors) Step 1
                t = True
                For j As Integer = 0 To UBound(obj.connectors(i)) Step 1
                    Call connectorPos(xx, yy, x, y, obj.connectors(i)(j))
                    If Not Connector(xx, yy) = 1 Then
                        t = False
                        Exit For
                    End If
                Next j
                If t Then Exit For
            Next i
            If Not t Then Return False
        End If
        Return MayPlace(m, x, y, free, obj.xSize, obj.ySize, obj.ground, obj.water, obj.race)
    End Function
    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, ByRef free(,) As Boolean, _
                              ByRef xSize As Integer, ByRef ySize As Integer, _
                              ByRef isGround As Boolean, ByRef isWater As Boolean, _
                              ByRef races As List(Of Integer)) As Boolean
        If x + xSize - 1 > m.xSize Or y + ySize - 1 > m.ySize Then Return False
        Dim x2 As Integer = x + xSize - 1
        Dim y2 As Integer = y + ySize - 1
        If Not CheckRaces(m.board(x, y).mapObject.objRace, races) Then Return False
        For j As Integer = y To y2 Step 1
            For i As Integer = x To x2 Step 1
                If Not m.board(i, j).passability.isBorder Or Not free(i, j) Then Return False
                If Not CheckSurface(m.board(i, j).surface.isWater, isGround, isWater) Then Return False
            Next i
        Next j
        Return True
    End Function
    Private Function CheckSurface(ByRef waterSurface As Boolean, ByRef isGround As Boolean, ByRef isWater As Boolean) As Boolean
        If waterSurface Then
            If Not isWater Then Return False
        Else
            If Not isGround Then Return False
        End If
        Return True
    End Function
    Private Function CheckRaces(ByRef mRaces As List(Of Integer), ByRef objRaces As List(Of Integer)) As Boolean
        For Each r As Integer In mRaces
            If objRaces.Contains(r) Then Return True
        Next r
        Return False
    End Function
#End Region
#Region "Place objects tools"
    Private Sub PlaceObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As MapObject)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, False)
    End Sub
    Private Sub PlaceObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As Landmark)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, False)
    End Sub
    Private Sub PlaceObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As PlateauObject, ByRef connectors(,) As Integer)
        Call PlaceObject(free, x, y, obj, connectors, 1)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, False)
    End Sub
    Private Sub PlaceObject(ByRef free(,) As Boolean, _
                          ByRef x As Integer, ByRef y As Integer, _
                          ByRef obj As PlateauObject, ByRef connectors(,) As Integer, _
                          ByRef whatAdd As Integer)
        Dim xx, yy As Integer
        For i As Integer = 0 To UBound(obj.connectors) Step 1
            For j As Integer = 0 To UBound(obj.connectors(i)) Step 1
                Call connectorPos(xx, yy, x, y, obj.connectors(i)(j))
                connectors(xx, yy) += whatAdd
            Next j
        Next i
    End Sub

    Private Sub PlaceObject(ByRef free(,) As Boolean, _
                            ByRef x As Integer, ByRef y As Integer, _
                            ByRef xSize As Integer, ByRef ySize As Integer, _
                            ByRef whatSet As Boolean)
        Dim x1 As Integer = x + xSize - 1
        Dim y1 As Integer = y + ySize - 1
        For j As Integer = y To y1 Step 1
            For i As Integer = x To x1 Step 1
                free(i, j) = whatSet
            Next i
        Next j
    End Sub
  
    Private Sub RemoveObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As MapObject)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, True)
    End Sub
    Private Sub RemoveObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As PlateauObject, ByRef connectors(,) As Integer)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, True)
        Call PlaceObject(free, x, y, obj, connectors, -1)
    End Sub
#End Region
#Region "ObjectWeight"
    Private Function ObjectWeight(ByRef m As Map, ByRef obj As MapObject, _
                                  ByRef x As Integer, ByRef y As Integer) As Double
        Return ObjectWeight(m, obj.name, x, y, obj.xSize * obj.ySize)
    End Function
    Private Function ObjectWeight(ByRef m As Map, ByRef obj As Landmark, _
                                  ByRef x As Integer, ByRef y As Integer) As Double
        Return ObjectWeight(m, obj.name, x, y, obj.xSize * obj.ySize)
    End Function
    Private Function ObjectWeight(ByRef m As Map, ByRef objName As String, _
                                  ByRef x As Integer, ByRef y As Integer, _
                                  ByRef sizeMultiplier As Double) As Double
        Dim w As Double = sizeMultiplier
        Dim minW As Double = 0.025
        Dim maxW As Double = 0.5
        Dim L As Integer = 12
        Dim dW As Double = (maxW - minW) / ((L * L + 1) ^ 2)
        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, L)
        Dim dx, dy As Integer
        For p As Integer = b.minY To b.maxY Step 1
            dy = p - y : dy = dy * dy + 1
            For q As Integer = b.minX To b.maxX Step 1
                If objName = m.board(q, p).mapObject.objectName Then
                    dx = q - x : dx = dx * dx + 1
                    w = w * (minW + dW * CDbl(dx * dy))
                End If
            Next q
        Next p
        Return w
    End Function
#End Region

    Private Sub PlaceAttendedObjects(ByRef m As Map)
        Dim objList() As MapObject
        Dim IDs As New List(Of Integer)
        Dim id As Integer
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID > DefMapObjects.Types.City And m.board(x, y).mapObject.objectID < DefMapObjects.Types.Mine Then
                    If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Vendor Then
                        objList = merchants
                    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mercenary Then
                        objList = mercenaries
                    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mage Then
                        objList = mages
                    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Trainer Then
                        objList = trainers
                    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Ruins Then
                        objList = ruins
                    Else
                        objList = Nothing
                    End If
                    IDs.Clear()
                    If IsNothing(objList) Then
                        Throw New Exception("Список объектов для objectID=" & m.board(x, y).mapObject.objectID & " пуст (см. DefMapObjects.Types)")
                    End If
                    For i As Integer = 0 To UBound(objList) Step 1
                        If CheckSurface(m.board(x, y).surface.isWater, objList(i).ground, objList(i).water) _
                        AndAlso CheckRaces(m.board(x, y).mapObject.objRace, objList(i).race) Then
                            IDs.Add(i)
                        End If
                    Next i
                    If IDs.Count > 0 Then
                        id = comm.RandomSelection(IDs, True)
                    Else
                        Throw New Exception("Нет ни одного подходящего объекта для objectID=" & m.board(x, y).mapObject.objectID & " (см. DefMapObjects.Types)")
                        id = -1
                    End If
                    m.board(x, y).mapObject.objectName = objList(id).name
                End If
            Next x
        Next y
    End Sub
#Region "Place towns"
    Private Sub PlaceCities(ByRef m As Map, ByRef settMap As Map.SettingsMap)
        Dim cityGroup As Dictionary(Of Integer, List(Of Point)) = MakeCityGroupsList(m)
        Dim locRace As New Dictionary(Of Integer, String)
        locRace.Add(0, "N")
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                    m.board(x, y).mapObject.objectName = comm.defValues.generatorRaceToCapitalID(comm.defValues.RaceNumberToRaceChar(m.board(x, y).mapObject.objRace.Item(0)))
                    locRace.Add(m.board(x, y).locID(0), comm.defValues.RaceNumberToRaceChar(m.board(x, y).mapObject.objRace.Item(0)))
                End If
            Next x
        Next y
        If cityGroup.Count > 0 Then
            For Each L As List(Of Point) In cityGroup.Values
                Dim town As String
                Dim level As Integer = m.board(L.Item(0).X, L.Item(0).Y).mapObject.City.level
                If level = 0 Then
                    If m.board(L.Item(0).X, L.Item(0).Y).locID(0) <= settMap.nRaces Then
                        level = 1
                    Else
                        Dim r As Double = comm.rndgen.PRand(0, 1)
                        If r > 0.3 Then
                            level = 1
                        ElseIf r > 0.1 Then
                            level = 2
                        Else
                            level = 3
                        End If
                    End If
                End If
                If level = 1 Then
                    town = DefMapObjects.townT1
                ElseIf level = 2 Then
                    town = DefMapObjects.townT2
                ElseIf level = 3 Then
                    town = DefMapObjects.townT3
                ElseIf level = 4 Then
                    town = DefMapObjects.townT4
                ElseIf level = 5 Then
                    town = DefMapObjects.townT5
                Else
                    Throw New Exception("Unexpected preset town level: " & m.board(L.Item(0).X, L.Item(0).Y).mapObject.City.level)
                End If
                For Each p As Point In L
                    m.board(p.X, p.Y).mapObject.objectName = town
                    m.board(p.X, p.Y).mapObject.City.level = level
                    m.board(p.X, p.Y).mapObject.City.race = locRace.Item(m.board(p.X, p.Y).mapObject.City.owner)
                Next p
            Next L
        End If
    End Sub
    Private Function MakeCityGroupsList(ByRef m As Map) As Dictionary(Of Integer, List(Of Point))
        Dim cityGroup As New Dictionary(Of Integer, List(Of Point))
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                    Dim g As Integer = m.board(x, y).groupID
                    If Not cityGroup.ContainsKey(g) Then cityGroup.Add(g, New List(Of Point))
                    cityGroup.Item(g).Add(New Point(x, y))
                End If
            Next x
        Next y
        Return cityGroup
    End Function
    Private Sub CorrectTownGuardsDesiredStats(ByRef m As Map)
        Dim cityGroup As Dictionary(Of Integer, List(Of Point)) = MakeCityGroupsList(m)
        Dim s As AllDataStructues.DesiredStats
        Dim citySize As Integer
        Dim cityName As String
        For Each g As Integer In cityGroup.Keys
            If m.groupStats.ContainsKey(-g) Then
                s = AllDataStructues.DesiredStats.Copy(m.groupStats.Item(-g))
                cityName = m.board(cityGroup.Item(g).Item(0).X, cityGroup.Item(g).Item(0).Y).mapObject.objectName
                citySize = CInt(cityName.Substring(cityName.Length - 1))
                If s.StackSize > citySize Then
                    s.ExpBarAverage = CInt((s.ExpBarAverage * s.StackSize) / citySize)
                    s.StackSize = citySize
                    If s.StackSize < 2 Then
                        s.MaxGiants = 0
                    ElseIf s.StackSize < 4 Then
                        s.MaxGiants = Math.Min(s.MaxGiants, 1)
                    ElseIf s.StackSize < 6 Then
                        s.MaxGiants = Math.Min(s.MaxGiants, 2)
                    Else
                        s.MaxGiants = Math.Min(s.MaxGiants, 3)
                    End If
                End If
                s.isInternalCityGuard = True
                m.groupStats.Remove(-g)
                m.groupStats.Add(-g, AllDataStructues.DesiredStats.Copy(s))
            End If
        Next g
    End Sub
#End Region
#Region "Place mines"
    Private Sub PlaceMines(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                           ByRef settLoc() As Map.SettingsLoc)
        Dim mineType(m.xSize, m.ySize) As String
        For i As Integer = 0 To UBound(m.Loc) Step 1
            If Not m.Loc(i).IsObtainedBySymmery Then
                If i < settMap.nRaces Then
                    Call PlaceMinesInRaceLoc(m, settMap, settLoc(i), m.Loc(i), mineType)
                Else
                    Call PlaceMinesInCommonLoc(m, settMap, settLoc(i), m.Loc(i), mineType)
                End If
            End If
        Next i
        'установить для шахт конкретный вид ресурсов
        Dim raceMana As Dictionary(Of Integer, AllDataStructues.Cost()) = RacesManaUsing(comm, raceSpells)
        Dim raceManaTier As Dictionary(Of Integer, String()) = ManaTier(raceMana)
        Dim IDs As New List(Of Integer)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not mineType(x, y) = "" Then
                    Dim types() As String = Nothing
                    Dim weights() As Double = Nothing
                    If mineType(x, y) = My.Resources.mineTypeGold Then
                        types = New String() {"gold"}
                        weights = New Double() {1}
                    ElseIf mineType(x, y) = My.Resources.mineTypeT1Mana Or mineType(x, y) = My.Resources.mineTypeT3Mana Then
                        Dim tier As Integer
                        If mineType(x, y) = My.Resources.mineTypeT1Mana Then
                            tier = 1
                        Else
                            tier = 3
                        End If
                        types = raceManaTier(m.board(x, y).mapObject.objRace(0))(tier).Split(CChar(" "))
                        ReDim weights(UBound(types))
                        For i As Integer = 0 To UBound(types) Step 1
                            If types(i) = "gold" Then
                                weights(i) = raceMana.Item(m.board(x, y).mapObject.objRace(0))(tier).Gold
                            ElseIf types(i) = "green" Then
                                weights(i) = raceMana.Item(m.board(x, y).mapObject.objRace(0))(tier).Green
                            ElseIf types(i) = "black" Then
                                weights(i) = raceMana.Item(m.board(x, y).mapObject.objRace(0))(tier).Black
                            ElseIf types(i) = "white" Then
                                weights(i) = raceMana.Item(m.board(x, y).mapObject.objRace(0))(tier).White
                            ElseIf types(i) = "red" Then
                                weights(i) = raceMana.Item(m.board(x, y).mapObject.objRace(0))(tier).Red
                            ElseIf types(i) = "blue" Then
                                weights(i) = raceMana.Item(m.board(x, y).mapObject.objRace(0))(tier).Blue
                            End If
                        Next i
                        If tier = 1 Then
                            Dim maxW As Double = weights.Max
                            For i As Integer = 0 To UBound(types) Step 1
                                If weights(i) < maxW Then weights(i) = 0
                            Next i
                        End If
                    ElseIf mineType(x, y) = My.Resources.mineTypeRandomMana Then
                        types = New String() {My.Resources.mineTypeRandomMana}
                        weights = New Double() {1}
                    End If
                    IDs.Clear()
                    For i As Integer = 0 To UBound(types) Step 1
                        IDs.Add(i)
                    Next i
                    Dim r As Integer = comm.RandomSelection(IDs, True)
                    m.board(x, y).mapObject.objectName = types(r)
                End If
            Next x
        Next y
    End Sub
    Private Sub PlaceMinesInRaceLoc(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, _
                                    ByRef Loc As Location, ByRef mineType(,) As String)
        Dim ListPos As New List(Of Point)
        Dim capitalPos As Point = Nothing
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID(0) = Loc.ID Then
                    If m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mine Then
                        ListPos.Add(New Point(x, y))
                    ElseIf m.board(x, y).mapObject.objectID = DefMapObjects.Types.Capital Then
                        capitalPos = New Point(x, y)
                    End If
                End If
            Next x
        Next y
        Dim pos() As Point = ListPos.ToArray
        Dim dist(UBound(pos)) As Integer
        For i As Integer = 0 To UBound(pos) Step 1
            dist(i) = capitalPos.SqDist(pos(i))
        Next i
        Array.Sort(dist, pos)
        Dim goldLimit As Double = CDbl(pos.Length) * settRaceLoc.maxGoldMines / (settRaceLoc.maxGoldMines + settRaceLoc.maxManaSources + 0.001)
        Dim manaLimit As Double = CDbl(pos.Length) * settRaceLoc.maxManaSources / (settRaceLoc.maxGoldMines + settRaceLoc.maxManaSources + 0.001)
        Dim manaCounter As Integer = 0
        For i As Integer = 0 To UBound(pos) Step 1
            If i = 0 And goldLimit > 0 Then
                Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeGold)
                goldLimit -= 1
            ElseIf i = 1 And manaLimit > 0 Then
                Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeT1Mana)
                manaLimit -= 1
                manaCounter += 1
            Else
                If PlaceGoldMine(goldLimit, manaLimit, settRaceLoc) Then
                    Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeGold)
                    goldLimit -= 1
                Else
                    If manaCounter > 1 And settMap.SpellsMaxLevel > 2 Then
                        Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeT3Mana)
                        manaCounter = 0
                    Else
                        Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeT1Mana)
                        manaCounter += 1
                    End If
                    manaLimit -= 1
                End If
            End If
        Next i
    End Sub
    Private Sub PlaceMinesInCommonLoc(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settCommLoc As Map.SettingsLoc, _
                                      ByRef Loc As Location, ByRef mineType(,) As String)
        Dim ListPos As New List(Of Point)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID(0) = Loc.ID And m.board(x, y).mapObject.objectID = DefMapObjects.Types.Mine Then ListPos.Add(New Point(x, y))
            Next x
        Next y
        Dim goldLimit As Double = CDbl(ListPos.Count) * settCommLoc.maxGoldMines / (settCommLoc.maxGoldMines + settCommLoc.maxManaSources + 0.001)
        Dim manaLimit As Double = CDbl(ListPos.Count) * settCommLoc.maxManaSources / (settCommLoc.maxGoldMines + settCommLoc.maxManaSources + 0.001)
        Dim r1 As Double
        Dim r2 As Double = settCommLoc.maxGoldMines / (settCommLoc.maxManaSources + 0.001)
        For Each p As Point In ListPos
            r1 = goldLimit / (manaLimit + 0.001)
            If PlaceGoldMine(goldLimit, manaLimit, settCommLoc) Then
                Call SetMineType(m, settMap.nRaces, mineType, p, My.Resources.mineTypeGold)
                goldLimit -= 1
            Else
                Call SetMineType(m, settMap.nRaces, mineType, p, My.Resources.mineTypeRandomMana)
                manaLimit -= 1
            End If
        Next p
    End Sub
    Private Function PlaceGoldMine(ByRef goldLimit As Double, ByRef manaLimit As Double, ByRef settLoc As Map.SettingsLoc) As Boolean
        If goldLimit > 0 Then
            Dim r1 As Double = goldLimit / (manaLimit + 0.001)
            Dim r2 As Double = settLoc.maxGoldMines / (settLoc.maxManaSources + 0.001)
            If goldLimit > 0 And manaLimit > 0 And goldLimit + manaLimit < 1 Then
                Dim R As Double = comm.rndgen.PRand(0, goldLimit + manaLimit)
                Return R < settLoc.maxGoldMines
            Else
                If r1 > r2 Or manaLimit <= 0 Then
                    Return True
                Else
                    Return False
                End If
            End If
        Else
            If manaLimit > 0 Then
                Return False
            Else
                Dim R As Double = comm.rndgen.PRand(0, settLoc.maxGoldMines + settLoc.maxManaSources)
                Return R < settLoc.maxGoldMines
            End If
        End If
    End Function
    Private Sub SetMineType(ByRef m As Map, ByRef nRaces As Integer, ByRef mineType(,) As String, _
                            ByRef p As Point, ByRef type As String)
        'Console.WriteLine(type)
        If m.symmID > -1 Then
            Dim pp() As Point = symm.ApplySymm(p, nRaces, m, 1)
            For i As Integer = 0 To UBound(pp) Step 1
                mineType(pp(i).X, pp(i).Y) = type
            Next i
        Else
            mineType(p.X, p.Y) = type
        End If
    End Sub
    Friend Shared Function RacesManaUsing(ByRef c As Common, ByRef rSpells() As AllDataStructues.Spell) As Dictionary(Of Integer, AllDataStructues.Cost())
        Dim spellsCostSumID As Integer = 0
        Dim maxSpellLevel As Integer = 5
        Dim res As New Dictionary(Of Integer, AllDataStructues.Cost())
        For Each r As Integer In c.LordsRace.Values
            If Not res.ContainsKey(r) Then
                res.Add(r, New AllDataStructues.Cost() {})
                ReDim res.Item(r)(maxSpellLevel)
            End If
        Next r
        For s As Integer = 0 To UBound(rSpells) Step 1
            If rSpells(s).researchCost.Count > 0 Then
                For Each L As String In rSpells(s).researchCost.Keys
                    Dim LRace As Integer = c.LordsRace.Item(L)
                    res.Item(LRace)(rSpells(s).level) += rSpells(s).researchCost.Item(L) + rSpells(s).castCost
                Next L
            End If
        Next s
        Dim remove As New List(Of Integer)
        For Each i As Integer In res.Keys
            For j As Integer = 1 To maxSpellLevel Step 1
                res.Item(i)(spellsCostSumID) += res.Item(i)(j)
            Next j
            If AllDataStructues.Cost.Sum(res.Item(i)(spellsCostSumID)) = 0 Then remove.Add(i)
        Next i
        For Each i As Integer In remove
            res.Remove(i)
        Next i
        Return res
    End Function
    Private Function ManaTier(ByRef raceMana As Dictionary(Of Integer, AllDataStructues.Cost())) As Dictionary(Of Integer, String())
        Dim res As New Dictionary(Of Integer, String())
        For Each i As Integer In raceMana.Keys
            res.Add(i, New String() {"", "", "", "", "", ""})
            For j As Integer = 1 To UBound(raceMana.Item(i)) Step 1
                If raceMana.Item(i)(j).Black > 0 Then res.Item(i)(j) &= "black "
                If raceMana.Item(i)(j).Blue > 0 Then res.Item(i)(j) &= "blue "
                If raceMana.Item(i)(j).Gold > 0 Then res.Item(i)(j) &= "gold "
                If raceMana.Item(i)(j).Green > 0 Then res.Item(i)(j) &= "green "
                If raceMana.Item(i)(j).Red > 0 Then res.Item(i)(j) &= "red "
                If raceMana.Item(i)(j).White > 0 Then res.Item(i)(j) &= "white "
                res.Item(i)(j) = res.Item(i)(j).Remove(res.Item(i)(j).Length - 1)
            Next j
            For j As Integer = 1 To UBound(raceMana.Item(i)) - 1 Step 1
                Dim s() As String = res.Item(i)(j).Split(CChar(" "))
                For q As Integer = 0 To UBound(s) Step 1
                    For k As Integer = j + 1 To UBound(raceMana.Item(i)) Step 1
                        If s(q).Length > 0 Then res.Item(i)(k) = res.Item(i)(k).Replace(s(q), "")
                    Next k
                Next q
            Next j
            For j As Integer = 1 To UBound(raceMana.Item(i)) Step 1
                res.Item(i)(j) = res.Item(i)(j).Trim
            Next j
        Next i
        Return res
    End Function
#End Region
#Region "Place plateau"
    Private Sub PlacePlateau(ByRef m As Map, ByRef free(,) As Boolean)
        Dim connectors(m.xSize, m.ySize) As Integer
        Dim IDs As List(Of Integer)
        Dim id As Integer
        Dim ok As Boolean
        For i As Integer = 0 To 2 * m.Loc.Length Step 1
            IDs = makeIDs(plateau, True, False)
            Do While IDs.Count > 0
                id = comm.RandomSelection(IDs, True)
                ok = TryToPlace(m, plateau(id), free, connectors)
                If ok Then
                    Exit Do
                Else
                    IDs.Remove(id)
                End If
            Loop
            If IDs.Count = 0 Then Exit For
        Next i
    End Sub
    Private Function TryToPlace(ByRef m As Map, ByRef basic As PlateauObject, _
                                ByRef free(,) As Boolean, ByRef connectors(,) As Integer) As Boolean

        'сначала пытаемся расположить базовый кусок (в каждом из возможных положений, пока не получится построить цепочку)
        'после успешного расположения базового куска пытаемся построить от него цепочку
        'если все получилось - возвращаем true
        Dim places As New List(Of Point)
        Dim res As New PlateauPlacingResult With {.n = -1}
        ReDim res.obj(maxChainLen), res.pos(maxChainLen)
        Dim id As Integer
        Dim ok As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If MayPlace(m, x, y, free, basic, Nothing) Then places.Add(New Point(x, y))
            Next x
        Next y

        Do While places.Count > 0
            id = comm.rndgen.RndPos(places.Count, True) - 1
            ok = MakeChain(m, basic, places(id).X, places(id).Y, free, connectors, res)
            If ok Then
                For i As Integer = 0 To res.n Step 1
                    m.board(res.pos(i).X, res.pos(i).Y).mapObject.objectName = res.obj(i).name
                Next i
                Return True
            Else
                places.RemoveAt(id)
            End If
        Loop
        Return False
    End Function
    Private Sub connectorPos(ByRef xOut As Integer, ByRef yOut As Integer, ByRef x As Integer, ByRef y As Integer, _
                             ByRef connectorRelativePos As Point)
        xOut = x + connectorRelativePos.X
        yOut = y + connectorRelativePos.Y
        'If connectorRelativePos.X > 0 Then xOut += obj.xSize - 1
        'If connectorRelativePos.Y > 0 Then yOut += obj.ySize - 1
    End Sub
    Private Function MakeChain(ByRef m As Map, ByRef obj As PlateauObject, _
                               ByRef x As Integer, ByRef y As Integer, _
                               ByRef free(,) As Boolean, ByRef connectors(,) As Integer, _
                               ByRef res As PlateauPlacingResult) As Boolean
        res.n += 1
        res.obj(res.n) = obj
        res.pos(res.n) = New Point(x, y)

        Call PlaceObject(free, x, y, obj, connectors)
        Dim x1 As Integer = Math.Max(x - maxPlateauSize, 0)
        Dim y1 As Integer = Math.Max(y - maxPlateauSize, 0)
        Dim x2 As Integer = Math.Min(x + obj.xSize + maxPlateauSize - 1, m.xSize)
        Dim y2 As Integer = Math.Min(y + obj.ySize + maxPlateauSize - 1, m.ySize)
        Dim placeMore As Boolean = False
        For j As Integer = y1 To y2 Step 1
            For i As Integer = x1 To x2 Step 1
                If connectors(i, j) = 1 Then
                    placeMore = True
                    i = x2
                    j = y2
                End If
            Next i
        Next j
        If Not placeMore Then Return True

        Dim IDs As List(Of Integer)
        Dim id, nx, ny As Integer
        If res.n < maxChainLen Then
            IDs = makeIDs(plateau, False, True)
        Else
            IDs = makeIDs(plateau, True, True)
        End If

        Do While IDs.Count > 0
            id = comm.rndgen.RndPos(IDs.Count, True) - 1
            nx = -1
            For j As Integer = y1 To y2 Step 1
                For i As Integer = x1 To x2 Step 1
                    If MayPlace(m, i, j, free, plateau(id), connectors) Then
                        nx = i
                        ny = j
                        i = x2
                        j = y2
                    End If
                Next i
            Next j
            If nx > -1 Then
                If MakeChain(m, plateau(id), nx, ny, free, connectors, res) Then
                    Return True
                Else
                    IDs.RemoveAt(id)
                End If
            Else
                IDs.RemoveAt(id)
            End If
        Loop
        Call RemoveObject(free, res.pos(res.n).X, res.pos(res.n).Y, res.obj(res.n), connectors)
        res.obj(res.n) = Nothing
        res.pos(res.n) = Nothing
        res.n -= 1
        Return False
    End Function
#End Region
#Region "Place mountains"
    Private Sub PlaceMouintains(ByRef m As Map, ByRef free(,) As Boolean)

        Dim Tmp(,) As Integer
        Dim sizeslist, IDs As New List(Of Integer)
        Dim pos As New List(Of Point)
        Dim tmpPos() As List(Of Point)
        For Each obj As MapObject In mountains
            If Not sizeslist.Contains(obj.xSize) Then sizeslist.Add(obj.xSize)
        Next obj
        Dim basicSize(sizeslist.Count - 1) As Integer
        Dim weight(UBound(basicSize)) As Double
        For s As Integer = 0 To sizeslist.Count - 1 Step 1
            basicSize(s) = sizeslist.Item(s)
            weight(s) = 1 / basicSize(s) ^ 2
        Next s
        sizeslist = Nothing
        Array.Sort(basicSize)
        Dim maxR As Integer
        Dim ismountain(m.xSize, m.ySize) As Boolean

        For s As Integer = UBound(basicSize) To 0 Step -1
            If basicSize(s) > 1 Then
                Dim n As Integer = Math.Max(1, CInt(0.5 * UBound(m.Loc))) * Math.Max(1, 6 - basicSize(s))
                For attempt As Integer = 0 To n Step 1
                    ReDim Tmp(2 * (m.xSize + 1), 2 * (m.ySize + 1))
                    ReDim tmpPos(UBound(Tmp, 2))
                    For y As Integer = 0 To UBound(Tmp, 2) Step 1
                        tmpPos(y) = New List(Of Point)
                    Next y
                    Call PlaceTmpMountain(basicSize(s), m.xSize, m.ySize, Tmp)
                    Dim minX, maxX, minY, maxY As Integer
                    minX = m.xSize : minY = m.ySize
                    maxX = minX + basicSize(s) - 1 : maxY = minY + basicSize(s) - 1

                    maxR = s
                    IDs.Clear()
                    For i As Integer = 0 To maxR - 1 Step 1
                        IDs.Add(i)
                    Next i
                    Dim k As Integer = 0
                    Do While k < 7
                        k += 1
                        Dim r As Integer = comm.RandomSelection(IDs, weight, True) 'comm.rndgen.RndPos(maxR, True) - 1
                        Parallel.For(0, UBound(Tmp, 2) + 1, _
                         Sub(y As Integer)
                             tmpPos(y).Clear()
                             For x As Integer = 0 To UBound(Tmp, 1) Step 1
                                 If MayPlaceMountainBlock(basicSize(r), x, y, Tmp, minX, maxX, minY, maxY) Then
                                     tmpPos(y).Add(New Point(x, y))
                                 End If
                             Next x
                         End Sub)
                        pos.Clear()
                        For y As Integer = 0 To UBound(Tmp, 2) Step 1
                            For Each p As Point In tmpPos(y)
                                pos.Add(p)
                            Next p
                        Next y
                        If pos.Count > 0 Then
                            Dim selected As Integer = comm.rndgen.RndPos(pos.Count, True) - 1
                            Call PlaceTmpMountain(basicSize(r), pos.Item(selected).X, pos.Item(selected).Y, Tmp)
                            minX = Math.Min(minX, pos.Item(selected).X)
                            minY = Math.Min(minY, pos.Item(selected).Y)
                            maxX = Math.Max(maxX, pos.Item(selected).X + basicSize(r) - 1)
                            maxY = Math.Max(maxY, pos.Item(selected).Y + basicSize(r) - 1)
                        Else
                            If r = 0 Then
                                Exit Do
                            Else
                                maxR = r
                                IDs.Clear()
                                For i As Integer = 0 To maxR - 1 Step 1
                                    IDs.Add(i)
                                Next i
                            End If
                        End If
                    Loop

                    pos.Clear()
                    For y As Integer = 0 To m.ySize Step 1
                        For x As Integer = 0 To m.xSize Step 1
                            If MayPlaceMountainBlock(m, free, x, y, Tmp, minX, maxX, minY, maxY) Then pos.Add(New Point(x, y))
                        Next x
                    Next y
                    If pos.Count > 0 Then
                        Dim selected As Integer = comm.rndgen.RndPos(pos.Count, True) - 1
                        Call PlaceMountainBlock(m, free, pos(selected).X, pos(selected).Y, Tmp, minX, maxX, minY, maxY, ismountain)
                    End If
                Next attempt
            End If
        Next s
        For s As Integer = UBound(basicSize) To 0 Step -1
            If basicSize(s) < 4 Then
                Dim j1 As Integer = m.ySize
                Dim j2 As Integer = 0
                Dim i1 As Integer = m.xSize
                Dim i2 As Integer = 0
                Dim s1 As Integer = -1
                For a As Integer = 0 To 3 Step 1
                    j1 = m.ySize - j1
                    j2 = m.ySize - j2
                    i1 = m.xSize - i1
                    i2 = m.xSize - i2
                    s1 = -s1
                    For j As Integer = j1 To j2 Step s1
                        For i As Integer = i1 To i2 Step s1
                            If MayPlaceSingleMountain(m, free, i, j, basicSize(s), ismountain) Then
                                Call PlaceSingleMountain(m, free, i, j, basicSize(s), ismountain)
                            End If
                        Next i
                    Next j
                Next a
            End If
        Next s
    End Sub
    Private Sub PlaceTmpMountain(ByRef size As Integer, ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer)
        For j As Integer = y To y + size - 1 Step 1
            For i As Integer = x To x + size - 1 Step 1
                tmp(i, j) = size
            Next i
        Next j
    End Sub
    Private Function MayPlaceMountainBlock(ByRef size As Integer, ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer, _
                                           ByRef minX As Integer, ByRef maxX As Integer, _
                                           ByRef minY As Integer, ByRef maxY As Integer) As Boolean
        If x + size - 1 > UBound(tmp, 1) Then Return False
        If y + size - 1 > UBound(tmp, 2) Then Return False

        If x + size < minX Then Return False
        If y + size < minY Then Return False
        If x > maxX + 1 Then Return False
        If y > maxY + 1 Then Return False

        Dim t As Boolean = False
        Dim e As Boolean = False
        For j As Integer = y To y + size - 1 Step 1
            For i As Integer = x To x + size - 1 Step 1
                If tmp(i, j) > 0 Then
                    Return False
                ElseIf Not t Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, UBound(tmp, 1), UBound(tmp, 2), 1)
                    For p As Integer = b.minY To b.maxY Step 1
                        For q As Integer = b.minX To b.maxX Step 1
                            If tmp(q, p) > size Then
                                t = True
                                Exit For
                            ElseIf tmp(q, p) = size Then
                                e = True
                            End If
                        Next q
                        If t Then Exit For
                    Next p
                End If
            Next i
        Next j
        If Not t And e AndAlso comm.rndgen.PRand(0, 1) < 0.01 Then Return True
        Return t
    End Function
    Private Function MayPlaceMountainBlock(ByRef m As Map, ByRef free(,) As Boolean, _
                                           ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer, _
                                           ByRef minX As Integer, ByRef maxX As Integer, _
                                           ByRef minY As Integer, ByRef maxY As Integer) As Boolean
        Dim dx As Integer = maxX - minX
        Dim dy As Integer = maxY - minY
        If x + dx > m.xSize Then Return False
        If y + dy > m.ySize Then Return False
        For j As Integer = 0 To dy Step 1
            For i As Integer = 0 To dx Step 1
                If tmp(minX + i, minY + j) > 0 Then
                    If Not TestCellForMountain(m, free, x + i, y + j) Then Return False
                End If
            Next i
        Next j
        Return True
    End Function
    Private Function TestCellForMountain(ByRef m As Map, ByRef free(,) As Boolean, _
                                         ByRef x As Integer, ByRef y As Integer) As Boolean
        If Not free(x, y) Then Return False
        If Not m.board(x, y).passability.isBorder Then Return False
        If m.board(x, y).surface.isWater Then
            Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
            For p As Integer = b.minY To b.maxY Step 1
                For q As Integer = b.minX To b.maxX Step 1
                    If m.board(q, p).surface.isWater And (m.board(q, p).stack.GuardLoc Or _
                                                  m.board(q, p).stack.ObjectGuard Or _
                                                  m.board(q, p).stack.PassGuardLoc) Then
                        Return False
                    End If
                Next q
            Next p
        End If
        Return True
    End Function
    Private Sub PlaceMountainBlock(ByRef m As Map, ByRef free(,) As Boolean, _
                                   ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer, _
                                   ByRef minX As Integer, ByRef maxX As Integer, _
                                   ByRef minY As Integer, ByRef maxY As Integer, _
                                   ByRef ismountain(,) As Boolean)
        Dim dx As Integer = maxX - minX
        Dim dy As Integer = maxY - minY
        Dim IDs As New List(Of Integer)
        For j As Integer = 0 To dy Step 1
            For i As Integer = 0 To dx Step 1
                If tmp(minX + i, minY + j) > 0 Then
                    Dim size As Integer = tmp(minX + i, minY + j)
                    For p As Integer = 0 To size - 1 Step 1
                        For q As Integer = 0 To size - 1 Step 1
                            tmp(minX + i + q, minY + j + p) = 0
                        Next q
                    Next p
                    Call PlaceSingleMountain(m, free, x + i, y + j, size, ismountain)
                End If
            Next i
        Next j
    End Sub
    Private Function MayPlaceSingleMountain(ByRef m As Map, ByRef free(,) As Boolean, _
                                            ByRef x As Integer, ByRef y As Integer, _
                                            ByRef size As Integer, _
                                            ByRef ismountain(,) As Boolean) As Boolean
        If x + size - 1 > m.xSize Then Return False
        If y + size - 1 > m.ySize Then Return False
        Dim t As Boolean = False
        For j As Integer = y To y + size - 1 Step 1
            For i As Integer = x To x + size - 1 Step 1
                If Not TestCellForMountain(m, free, i, j) Then Return False
                If Not t Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                    For p As Integer = b.minY To b.maxY Step 1
                        For q As Integer = b.minX To b.maxX Step 1
                            If ismountain(q, p) Then
                                t = True
                                Exit For
                            End If
                        Next q
                        If t Then Exit For
                    Next p
                End If
            Next i
        Next j
        If t Then
            Dim r As Double = comm.rndgen.PRand(0, 1)
            Dim d As Double = Math.Max(0.075 - 0.005 * size, 0.025)
            Return r < d
        Else
            Return False
        End If
    End Function
    Private Sub PlaceSingleMountain(ByRef m As Map, ByRef free(,) As Boolean, _
                                    ByRef x As Integer, ByRef y As Integer, _
                                    ByRef size As Integer, _
                                    ByRef ismountain(,) As Boolean)
        For p As Integer = 0 To size - 1 Step 1
            For q As Integer = 0 To size - 1 Step 1
                free(x + q, y + p) = False
                m.board(x + q, y + p).surface.isWater = False
                If Not IsNothing(ismountain) Then ismountain(x + q, y + p) = True
            Next q
        Next p
        Dim IDs As New List(Of Integer)
        Dim weight(UBound(mountains)) As Double
        For p As Integer = 0 To 1 Step 1
            For n As Integer = 0 To UBound(mountains) Step 1
                If mountains(n).xSize = size AndAlso (p = 1 OrElse CheckRaces(m.board(x, y).mapObject.objRace, mountains(n).race)) Then
                    IDs.Add(n)
                    weight(n) = ObjectWeight(m, mountains(n), x, y)
                End If
            Next n
            If IDs.Count > 0 Then Exit For
        Next p
        Dim selected As Integer = comm.RandomSelection(IDs, weight, True)
        m.board(x, y).mapObject.objectName = mountains(selected).name
    End Sub
#End Region
#Region "Place other objects: houses, towers, obelisks, breaches etc."
    Private Sub PlaceOtherObjects(ByRef m As Map, ByRef free(,) As Boolean)
        Dim IDs As New List(Of Integer)
        Dim weight(UBound(objects)) As Double
        Dim nextloop As Boolean = True
        Dim k As Integer = 0
        Dim f(,) As Boolean = free
        Dim tmpm As Map = m
        Do While nextloop And k < 5
            nextloop = False
            For y As Integer = 0 To tmpm.ySize Step 1
                For x As Integer = 0 To tmpm.xSize Step 1
                    If f(x, y) And tmpm.board(x, y).passability.isBorder Then
                        nextloop = True
                        IDs.Clear()
                        Dim xx As Integer = x
                        Dim yy As Integer = y
                        Parallel.For(0, objects.Length, _
                         Sub(n As Integer)
                             If MayPlace(tmpm, xx, yy, f, objects(n)) Then
                                 weight(n) = ObjectWeight(tmpm, objects(n), xx, yy)
                             Else
                                 weight(n) = -1
                             End If
                         End Sub)
                        For n As Integer = 0 To UBound(objects) Step 1
                            If weight(n) >= 0 Then IDs.Add(n)
                        Next n
                        If IDs.Count > 0 Then
                            Dim selected As Integer = comm.RandomSelection(IDs, weight, True)
                            tmpm.board(x, y).mapObject.objectName = objects(selected).name
                            Call PlaceObject(f, x, y, objects(selected))
                        End If
                    End If
                Next x
            Next y
            k += 1
        Loop
        Parallel.For(0, tmpm.ySize + 1,
         Sub(y As Integer)
             For x As Integer = 0 To tmpm.xSize Step 1
                 If f(x, y) And tmpm.board(x, y).passability.isBorder Then
                     If IDs.Count > 0 Then
                         'f(x, y) = False
                         'tmpm.board(x, y).mapObject.objectName = "MOMNE0100"
                         Call PlaceSingleMountain(tmpm, f, x, y, 1, Nothing)
                     End If
                 End If
             Next x
         End Sub)
        free = f
        m = tmpm
    End Sub
#End Region

#Region "Старые версии генераторов контента торговцев"
    'Private Sub AddSpells(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc)
    '    Dim maxGroupID As Integer = ImpenetrableMeshGen.GetMaxGroupID(m)
    '    Dim objList As Dictionary(Of Integer, List(Of Point)) = FindGroupsOfObjects(m, 5)
    '    Dim levels As List(Of Integer)
    '    Dim r, n As Integer
    '    Dim setNewGroup As Boolean
    '    For Each g As Integer In objList.Keys
    '        Dim pLocID As Integer = pointLoc(m, objList.Item(g).Item(0))
    '        If pLocID <= settMap.nRaces Then
    '            setNewGroup = False
    '            levels = SelectSpellsLevel(settLoc(pLocID - 1))
    '            For Each p As Point In objList.Item(g)
    '                r = m.board(p.X, p.Y).mapObject.objRace.Item(0)
    '                If setNewGroup Then
    '                    maxGroupID += 1
    '                    n = maxGroupID
    '                    m.board(p.X, p.Y).groupID = maxGroupID
    '                Else
    '                    setNewGroup = True
    '                    n = g
    '                End If
    '                m.groupStats.Add(n, New AllDataStructues.DesiredStats With {.shopContent = SelectSpells(settLoc(pLocID - 1), r, levels)})
    '            Next p
    '        Else
    '            levels = SelectSpellsLevel(settLoc(pLocID - 1))
    '            m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectSpells(settLoc(pLocID - 1), -1, levels)})
    '        End If
    '    Next g
    'End Sub
    'Private Function SelectSpells(ByRef settLoc As Map.SettingsLoc, ByRef raceID As Integer, ByRef levels As List(Of Integer)) As List(Of String)
    '    Dim res As New List(Of String)
    '    Dim v, r As String
    '    If raceID = -1 Then
    '        r = "R"
    '    Else
    '        r = raceIdToString.Item(raceID)
    '    End If
    '    For Each L As Integer In levels
    '        v = L.ToString & r
    '        If settLoc.mageGlobalSpellsEnabled Then
    '            v &= "T"
    '        Else
    '            v &= "F"
    '        End If
    '        res.Add(v)
    '    Next L
    '    Return res
    'End Function
    'Private Function SelectSpellsLevel(ByRef settLoc As Map.SettingsLoc) As List(Of Integer)
    '    Dim res As New List(Of Integer)
    '    For i As Integer = 1 To settLoc.mageSpellsCount Step 1
    '        res.Add(comm.rndgen.RndInt(settLoc.mageSpellsMinLevel, settLoc.mageSpellsMaxLevel, True))
    '    Next i
    '    Return res
    'End Function
    'Private Function FindGroupsOfObjects(ByRef m As Map, ByRef objType As Integer) As Dictionary(Of Integer, List(Of Point))
    '    Dim res As New Dictionary(Of Integer, List(Of Point))
    '    For y As Integer = 0 To m.ySize Step 1
    '        For x As Integer = 0 To m.xSize Step 1
    '            If m.board(x, y).mapObject.objectID = objType Then
    '                If Not res.ContainsKey(m.board(x, y).groupID) Then res.Add(m.board(x, y).groupID, New List(Of Point))
    '                res.Item(m.board(x, y).groupID).Add(New Point(x, y))
    '            End If
    '        Next x
    '    Next y
    '    Return res
    'End Function
    'Private Function pointLoc(ByRef m As Map, ByRef p As Point) As Integer
    '    Return m.board(p.X, p.Y).locID(0)
    'End Function

    'Private Sub AddMercenaries(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc)
    '    Dim objList As Dictionary(Of Integer, List(Of Point)) = FindGroupsOfObjects(m, 4)
    '    For Each g As Integer In objList.Keys
    '        Dim pLocID As Integer = pointLoc(m, objList.Item(g).Item(0))
    '        m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectMercenaries(settLoc(pLocID - 1))})
    '    Next g
    'End Sub
    'Private Function SelectMercenaries(ByRef settLoc As Map.SettingsLoc) As List(Of String)
    '    Dim res As New List(Of String)
    '    For i As Integer = 1 To settLoc.mercenariesCount Step 1
    '        res.Add(comm.rndgen.RndInt(settLoc.mercenariesMinExpBar, settLoc.mercenariesMaxExpBar, True).ToString)
    '    Next i
    '    Return res
    'End Function

    'Private Sub AddMerchantItems(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settLoc() As Map.SettingsLoc)
    '    Dim objList As Dictionary(Of Integer, List(Of Point)) = FindGroupsOfObjects(m, 3)
    '    For Each g As Integer In objList.Keys
    '        Dim pLocID As Integer = pointLoc(m, objList.Item(g).Item(0))
    '        m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectMerchantItems(settLoc(pLocID - 1))})
    '    Next g
    'End Sub
    'Private Function SelectMerchantItems(ByRef settLoc As Map.SettingsLoc) As List(Of String)
    '    Dim res As New List(Of String)
    '    Dim sum, v As Integer
    '    If settLoc.merchMinConsumableItemCost + settLoc.merchMaxConsumableItemCost = 0 Then Return res
    '    Do While sum < settLoc.merchItemsCost
    '        v = comm.rndgen.RndInt(settLoc.merchMinItemCost, settLoc.merchMaxItemCost, True)
    '        If sum + v > settLoc.merchItemsCost Then v = settLoc.merchItemsCost - sum
    '        sum += v
    '        res.Add(v.ToString)
    '    Loop
    '    Return res
    'End Function
#End Region

End Class

Public Class ObjectsContentSet

    Protected Friend randStack As RandStack
    Private manaSourcesTypes() As String = New String() {DefMapObjects.mineGreen, DefMapObjects.mineBlack, DefMapObjects.mineWhite, _
                                                         DefMapObjects.mineRed, DefMapObjects.mineBlue}

    Friend spells, excludedSpells As New Dictionary(Of String, AllDataStructues.Spell)

    Delegate Function getSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)

    Private mines As New Dictionary(Of String, String)

    Friend excludedItemType As New List(Of Integer)

    Private typeMinCost(-1), typeMaxCost(-1) As Integer

    ''' <param name="RStack">Инициализированный класс</param>
    Public Sub New(ByRef RStack As RandStack)

        randStack = RStack

        Dim nTypes As Integer
        For Each item As AllDataStructues.Item In randStack.AllItems
            nTypes = Math.Max(nTypes, item.type)
        Next item
        ReDim typeMinCost(nTypes), typeMaxCost(nTypes)
        For i As Integer = 0 To nTypes Step 1
            typeMinCost(i) = Integer.MaxValue
        Next i

        For Each item As AllDataStructues.Item In randStack.AllItems
            If Not randStack.comm.IsExcluded(item) Then
                Dim sum As Integer = AllDataStructues.Cost.Sum(randStack.LootCost(item))
                If sum > 0 Then
                    typeMinCost(item.type) = Math.Min(typeMinCost(item.type), sum)
                    typeMaxCost(item.type) = Math.Max(typeMaxCost(item.type), sum)
                End If
            End If
        Next item

        For Each item As AllDataStructues.Spell In randStack.AllSpells
            If Not randStack.comm.IsExcluded(item) Then
                spells.Add(item.spellID.ToUpper, AllDataStructues.Spell.Copy(item))
            Else
                excludedSpells.Add(item.spellID.ToUpper, AllDataStructues.Spell.Copy(item))
            End If
        Next item

        excludedItemType.AddRange(New Integer() {GenDefaultValues.ItemTypes.jewel, GenDefaultValues.ItemTypes.special})

        mines.Add("GOLD", DefMapObjects.mineGold)
        mines.Add("GREEN", DefMapObjects.mineGreen)
        mines.Add("BLACK", DefMapObjects.mineBlack)
        mines.Add("WHITE", DefMapObjects.mineWhite)
        mines.Add("RED", DefMapObjects.mineRed)
        mines.Add("BLUE", DefMapObjects.mineBlue)

    End Sub

    Private Sub AddToLog(ByRef log As Log, ByRef LogID As Integer, ByRef Msg As String)
        If LogID > -1 Then
            Call log.MAdd(LogID, Msg)
        Else
            Call log.Add(Msg)
        End If
    End Sub

    ''' <summary>Определит тип шахты</summary>
    ''' <param name="mineObjectName">Название шахты, как его выдал генератор</param>
    Public Function SetMineType(ByVal mineObjectName As String) As String
        Dim m As String = mineObjectName.ToUpper
        If m = My.Resources.mineTypeRandomMana.ToUpper Then
            Dim r As Integer = randStack.comm.rndgen.RndPos(manaSourcesTypes.Length, True) - 1
            Return manaSourcesTypes(r)
        Else
            If mines.ContainsValue(m) Then
                Return m
            Else
                Return mines.Item(m)
            End If
        End If
    End Function

#Region "Object content creation"
#Region "Mage merchant"
    ''' <summary>Создаст список заклинаний</summary>
    ''' <param name="d">Желаемые параметры доступных заклинаний. Имеет значение только .shopContent</param>
    ''' <param name="AllManaSources">Список источников маны на карте (см. DefMapObjects)</param>
    ''' <param name="log">Лог для записей результатов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function MakeSpellsList(ByRef d As AllDataStructues.DesiredStats, ByRef AllManaSources As List(Of String), _
                                   ByRef log As Log,
                                   Optional ByVal LogID As Integer = -1) As List(Of String)

        Call AddToLog(log, LogID, "----Spells creation started----")

        Dim availMana As New AllDataStructues.Cost
        If AllManaSources.Contains(DefMapObjects.mineGreen) Then availMana.Green = 1
        If AllManaSources.Contains(DefMapObjects.mineBlack) Then availMana.Black = 1
        If AllManaSources.Contains(DefMapObjects.mineWhite) Then availMana.White = 1
        If AllManaSources.Contains(DefMapObjects.mineRed) Then availMana.Red = 1
        If AllManaSources.Contains(DefMapObjects.mineBlue) Then availMana.Blue = 1

        Dim races() As String = New String() {My.Resources.spellRandomRace.ToUpper}
        For Each r As String In randStack.comm.defValues.playableRaces
            ReDim Preserve races(races.Length)
            races(UBound(races)) = randStack.comm.defValues.RaceNumberToRaceChar(randStack.comm.RaceIdentifierToSubrace(r))
        Next r

        Dim res, slist As New List(Of String)
        Dim txt As String = ""

        For Each L As String In d.shopContent
            If log.IsEnabled Then txt = "In: " & L & " -> "
            If L.Length = 10 AndAlso (L.Substring(0, 1).ToUpper = "G" And L.Substring(4, 2).ToUpper = "SS") Then
                If Not res.Contains(L.ToUpper) Then
                    res.Add(L.ToUpper)
                    If log.IsEnabled Then txt &= L.ToUpper
                Else
                    txt = ""
                End If
            ElseIf IsNumeric(L) Then
                slist.Clear()
                Dim type As Integer = CInt(L)
                For Each s As AllDataStructues.Spell In spells.Values
                    If Not res.Contains(s.spellID) AndAlso AddSpell(-1, -1, True, availMana, False, s) Then
                        If type > -1 Then
                            If s.category = type Then slist.Add(s.spellID)
                        Else
                            slist.Add(s.spellID)
                        End If
                    End If
                Next s
                Dim selected As String
                If slist.Count > 0 Then
                    selected = slist.Item(randStack.comm.rndgen.RndPos(slist.Count, True) - 1)
                Else
                    selected = ""
                End If
                If Not selected = "" Then res.Add(selected)
                If log.IsEnabled Then txt &= msgToLog(selected)
            ElseIf L.Contains("#") Then
                Dim s() As String = L.Split(CChar("#"))
                Dim type As Integer = CInt(s(0))
                Dim prop As String = s(1)
                Dim selected As String = SelectSpell(prop, type, races, availMana, res)
                If Not selected = "" Then res.Add(selected)
                If log.IsEnabled Then txt &= msgToLog(selected)
            Else
                Dim selected As String = SelectSpell(L, -1, races, availMana, res)
                If Not selected = "" Then res.Add(selected)
                If log.IsEnabled Then txt &= msgToLog(selected)
            End If
            If Not txt = "" Then Call AddToLog(log, LogID, txt)
        Next L

        Call AddToLog(log, LogID, "----Spells creation ended----")

        Return res
    End Function
    Private Function msgToLog(ByRef spellID As String) As String
        If Not spellID = "" Then
            Dim txt As String = spells.Item(spellID).spellID & " - " & _
                                spells.Item(spellID).name & " - " & _
                                "Lvl: " & spells.Item(spellID).level & " - " & _
                                "Cat: " & spells.Item(spellID).category
            Return txt
        Else
            Return "nothing"
        End If
    End Function
    Private Function SelectSpell(ByRef sProperties As String, ByRef type As Integer, ByRef races() As String, _
                                 ByRef availMana As AllDataStructues.Cost, ByRef res As List(Of String)) As String

        Dim level, race As Integer
        Dim mass, ignoreAvailMana As Boolean
        Dim slist As New List(Of String)
        Dim rlist() As Integer

        Dim L As String = sProperties.ToUpper
        If L.Contains("T") Then
            mass = True
            L = L.Replace("T", "")
        Else
            mass = False
            L = L.Replace("F", "")
        End If
        race = -2
        For Each r As String In races
            If L.Contains(r) Then
                If r.ToUpper = My.Resources.spellRandomRace.ToUpper Then
                    race = -1
                Else
                    race = randStack.comm.RaceIdentifierToSubrace(r)
                End If
                L = L.Replace(r, "")
                Exit For
            End If
        Next r
        level = CInt(L)
        ignoreAvailMana = False
        rlist = New Integer() {race, -1}
        For rr As Integer = 0 To UBound(rlist) Step 1
            For p As Integer = 0 To 1 Step 1
                For spellLevel As Integer = level To 0 Step -1
                    For Each s As AllDataStructues.Spell In spells.Values
                        If Not res.Contains(s.spellID) AndAlso AddSpell(spellLevel, rlist(rr), mass, _
                                                                        availMana, ignoreAvailMana, s) Then
                            If type > -1 Then
                                If s.category = type Then slist.Add(s.spellID)
                            Else
                                slist.Add(s.spellID)
                            End If
                        End If
                    Next s
                    If slist.Count > 0 Then Exit For
                Next spellLevel
                ignoreAvailMana = Not ignoreAvailMana
                If slist.Count > 0 Then Exit For
            Next p
            If slist.Count > 0 Then Exit For
        Next rr
        If slist.Count > 0 Then
            Return slist.Item(randStack.comm.rndgen.RndPos(slist.Count, True) - 1)
        Else
            Return ""
        End If
    End Function
    Private Function AddSpell(ByRef level As Integer, ByRef race As Integer, ByRef mass As Boolean, _
                              ByRef avail As AllDataStructues.Cost, ByRef ignoreAvail As Boolean, ByRef s As AllDataStructues.Spell) As Boolean
        If randStack.comm.IsExcluded(s) Then Return False

        If Not mass And s.area > 998 Then Return False
        If level > 0 And Not s.level = level Then Return False

        If Not ignoreAvail Then
            If s.castCost.Black > 0 And avail.Black = 0 Then Return False
            If s.castCost.Blue > 0 And avail.Blue = 0 Then Return False
            If s.castCost.Green > 0 And avail.Green = 0 Then Return False
            If s.castCost.Red > 0 And avail.Red = 0 Then Return False
            If s.castCost.White > 0 And avail.White = 0 Then Return False
        End If

        If race > -1 Then
            If IsNothing(s.researchCost) Then Return False
            Dim ok As Boolean = False
            For Each lord As String In s.researchCost.Keys
                If randStack.comm.LordsRace(lord) = race Then
                    ok = True
                    Exit For
                End If
            Next lord
            If Not ok Then Return False
        End If
        Return True
    End Function
#End Region
#Region "Units merchant"
    ''' <summary>Создаст список наемников</summary>
    ''' <param name="d">Желаемые параметры доступных наемников. Имеет значение только .shopContent</param>
    ''' <param name="log">Лог для записей результатов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function MakeMercenariesList(ByRef d As AllDataStructues.DesiredStats, _
                                        ByRef log As Log,
                                        Optional ByVal LogID As Integer = -1) As List(Of String)

        Call AddToLog(log, LogID, "----Mercenaries creation started----")

        Dim res As New List(Of String)
        Dim selection As New List(Of Integer)
        Dim txt As String = ""

        For Each v As String In d.shopContent
            If log.IsEnabled Then txt = "In: " & v & " -> "
            If IsNumeric(v) Then
                Dim bar As Integer = CInt(v)
                Dim selected As Integer = SelectMercenary(bar, -1, res)
                If selected > -1 Then res.Add(randStack.AllUnits(selected).unitID)
                If log.IsEnabled Then txt &= msgToLog(selected)
            ElseIf randStack.comm.RaceIdentifierToSubrace(v, False) > -1 Then
                selection.Clear()
                Dim selected As Integer = -1
                Dim race As Integer = randStack.comm.RaceIdentifierToSubrace(v)
                For u As Integer = 0 To UBound(randStack.AllUnits) Step 1
                    If randStack.comm.IsAppropriateFighter(randStack.AllUnits(u)) Then
                        If Not res.Contains(randStack.AllUnits(u).unitID) Then
                            If race = randStack.AllUnits(u).race Then selection.Add(u)
                        End If
                    End If
                Next u
                If selection.Count > 0 Then
                    selected = selection.Item(randStack.comm.rndgen.RndPos(selection.Count, True) - 1)
                    res.Add(randStack.AllUnits(selected).unitID)
                End If
                If log.IsEnabled Then txt &= msgToLog(selected)
            ElseIf v.Contains("#") Then
                Dim s() As String = v.Split(CChar("#"))
                Dim race As Integer
                If IsNumeric(s(0)) Then
                    race = CInt(s(0))
                Else
                    race = randStack.comm.RaceIdentifierToSubrace(s(0))
                End If
                Dim bar As Integer = CInt(s(1))
                Dim selected As Integer = SelectMercenary(bar, race, res)
                If selected > -1 Then res.Add(randStack.AllUnits(selected).unitID)
                If log.IsEnabled Then txt &= msgToLog(selected)
            Else
                If Not res.Contains(v.ToUpper) Then
                    res.Add(v)
                    If log.IsEnabled Then txt &= v.ToUpper
                Else
                    txt = ""
                End If
            End If
            If Not txt = "" Then Call AddToLog(log, LogID, txt)
        Next v

        Call AddToLog(log, LogID, "----Mercenaries creation ended----")

        Return res
    End Function
    Private Function msgToLog(ByRef unitID As Integer) As String
        If unitID > 0 Then
            Dim unit As AllDataStructues.Unit = randStack.AllUnits(unitID)
            Dim txt As String = unit.unitID & " - " & unit.name & " - " & randStack.comm.defValues.RaceNumberToRaceChar(unit.race) & " - " & unit.EXPnext
            If unit.small Then
                txt &= " (small)"
            Else
                txt &= " (big)"
            End If
            Return txt
        Else
            Return "nothing"
        End If
    End Function
    Private Function SelectMercenary(ByRef bar As Integer, ByRef race As Integer, ByRef added As List(Of String)) As Integer

        Dim selection As New List(Of Integer)

        Dim tolerance As Integer
        Dim dtolerance As Integer = 50
        Dim oneMore As Boolean = False
        Dim add As Boolean

        Do While (selection.Count = 0 Or oneMore) And tolerance <= 10000
            tolerance += dtolerance
            For u As Integer = 0 To UBound(randStack.AllUnits) Step 1
                If randStack.comm.IsAppropriateFighter(randStack.AllUnits(u)) Then
                    If Not added.Contains(randStack.AllUnits(u).unitID) Then
                        add = False
                        If randStack.AllUnits(u).small Then
                            If Math.Abs(randStack.AllUnits(u).EXPnext - bar) <= tolerance Then add = True
                        Else
                            If Math.Abs(randStack.AllUnits(u).EXPnext - 2 * bar) <= 2 * tolerance Then add = True
                        End If
                        If add And race > -1 Then
                            If Not race = randStack.AllUnits(u).race Then add = False
                        End If
                        If add Then selection.Add(u)
                    End If
                End If
            Next u
            If selection.Count > 0 Then oneMore = Not oneMore
        Loop
        'If selection.Count = 0 Then Throw New Exception("Не могу выбрать юнита в качестве наемника. Планка опыта: " & bar.ToString)
        If selection.Count > 0 Then
            Dim r As Integer = selection.Item(randStack.comm.rndgen.RndPos(selection.Count, True) - 1)
            Return r
        Else
            Return -1
        End If
    End Function
#End Region
#Region "Units merchant"
    ''' <summary>Создаст список предметов</summary>
    ''' <param name="d">Желаемые параметры доступных предметов. Имеет значение только .shopContent и .IGen.
    ''' IGen используется только при генерации по цене</param>
    ''' <param name="TypeCostRestriction">Ключ - тип предмета, Значение - ограничение стоимости. Игнорируется, если массив неинициализирован</param>
    ''' <param name="log">Лог для записей результатов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function MakeMerchantItemsList(ByRef d As AllDataStructues.DesiredStats, _
                                          ByRef TypeCostRestriction As Dictionary(Of Integer, AllDataStructues.Restriction), _
                                          ByRef log As Log, _
                                          Optional ByVal LogID As Integer = -1) As List(Of String)

        Call AddToLog(log, LogID, "----Alternative loot creation started----")
        'Call d.IGen.typesFilter.Initialize() - в генераторе товаров торговца есть свой механизм строгого сохранения типов
        Dim txt As String = ""
        Dim res As New List(Of String)
        Dim selection As New List(Of Integer)
        Dim dCost As Integer = 0
        Dim itemID As Integer = -1
        For Each v As String In d.shopContent
            If log.IsEnabled Then txt = "In: " & v
            If IsNumeric(v) Then
                Dim bar As Integer = CInt(v)
                itemID = SelectItem(bar, -1, dCost, d, res.Count, TypeCostRestriction)
                If itemID > -1 Then res.Add(randStack.AllItems(itemID).itemID)
                If log.IsEnabled Then txt &= msgToLog(itemID, dCost, True)
            ElseIf randStack.comm.itemTypeID.ContainsKey(v.ToUpper) Then
                selection.Clear()
                Dim type As Integer = randStack.comm.itemTypeID.Item(v.ToUpper)
                For u As Integer = 0 To UBound(randStack.AllItems) Step 1
                    If randStack.comm.IsAppropriateItem(randStack.AllItems(u)) Then
                        If randStack.AllItems(u).itemCost.Gold > 0 AndAlso randStack.LootCost(randStack.AllItems(u)).Gold = randStack.AllItems(u).itemCostSum Then
                            If type = randStack.AllItems(u).type AndAlso randStack.ItemFilter(d.IGen, randStack.AllItems(u), True) Then
                                If randStack.ItemFilter(TypeCostRestriction, randStack.AllItems(u)) Then
                                    selection.Add(u)
                                End If
                            End If
                        End If
                    End If
                Next u
                'If selection.Count = 0 Then Throw New Exception("Не могу выбрать предмет в качестве товара. Тип: " & v)
                If selection.Count > 0 Then
                    itemID = randStack.comm.RandomSelection(selection, randStack.multiplierItemsWeight, True)
                    res.Add(randStack.AllItems(itemID).itemID)
                Else
                    itemID = -1
                End If
                If log.IsEnabled Then txt &= msgToLog(itemID, dCost, False)
            ElseIf v.Contains("#") Then
                Dim s() As String = v.Split(CChar("#"))
                Dim type As Integer
                If IsNumeric(s(0)) Then
                    type = CInt(s(0))
                Else
                    type = randStack.comm.itemTypeID.Item(s(0).ToUpper)
                End If
                Dim bar As Integer = CInt(s(1))
                itemID = SelectItem(bar, type, dCost, d, res.Count, TypeCostRestriction)
                If itemID = -1 Then
                    If log.IsEnabled Then txt &= " (ignore type) "
                    itemID = SelectItem(bar, -1, dCost, d, res.Count, TypeCostRestriction)
                End If
                If itemID > -1 Then res.Add(randStack.AllItems(itemID).itemID)
                txt &= msgToLog(itemID, dCost, True)
            Else
                res.Add(v.ToUpper)
                If log.IsEnabled Then txt &= " -> " & v.ToUpper
            End If
            Call AddToLog(log, LogID, txt)
        Next v

        Call AddToLog(log, LogID, "----Alternative loot creation ended----")

        Return res
    End Function
    Private Function msgToLog(ByRef itemID As Integer, ByRef dCost As Integer, ByRef addDeltaCost As Boolean) As String
        Dim txt As String = ""
        If addDeltaCost Then txt &= " deltaCost: " & dCost
        txt &= " -> "
        If itemID > -1 Then
            txt &= randStack.AllItems(itemID).itemID & " - " & randStack.AllItems(itemID).name & " - " & randStack.AllItems(itemID).itemCost.Gold
        Else
            txt &= "nothong"
        End If
        Return txt
    End Function
    Private Function SelectItem(ByRef bar As Integer, ByRef type As Integer, ByRef dCost As Integer, _
                                ByRef d As AllDataStructues.DesiredStats, ByRef addedCount As Integer, _
                                ByRef TypeCostRestriction As Dictionary(Of Integer, AllDataStructues.Restriction)) As Integer
        Dim selection As New List(Of Integer)
        Dim correctedBar As Integer = bar + CInt(dCost / Math.Max(d.shopContent.Count - addedCount, 1))
        If correctedBar <= 0 Then
            dCost += bar
            Return -1
        End If
        Dim dtolerance As Integer = 100
        Dim tolerance As Integer
        Dim oneMore As Boolean = False
        Dim add As Boolean

        tolerance = 0
        Do While (selection.Count = 0 Or oneMore) And tolerance <= 10000
            tolerance += dtolerance
            selection.Clear()
            For u As Integer = 0 To UBound(randStack.AllItems) Step 1
                If randStack.comm.IsAppropriateItem(randStack.AllItems(u)) Then
                    If randStack.AllItems(u).itemCost.Gold > 0 AndAlso randStack.LootCost(randStack.AllItems(u)).Gold = randStack.AllItems(u).itemCostSum Then
                        If Math.Abs(randStack.LootCost(randStack.AllItems(u)).Gold - correctedBar) <= tolerance Then
                            If type < 0 Then
                                add = Not excludedItemType.Contains(randStack.AllItems(u).type)
                                If add Then add = randStack.ItemFilter(d.IGen, randStack.AllItems(u), True)
                            Else
                                add = (type = randStack.AllItems(u).type)
                            End If
                            If add AndAlso Not randStack.ItemFilter(TypeCostRestriction, randStack.AllItems(u)) Then add = False
                            If add Then selection.Add(u)
                        End If
                    End If
                End If
            Next u
            If selection.Count > 0 Then oneMore = Not oneMore
        Loop
        dCost += bar
        If selection.Count > 0 Then
            Dim r As Integer = randStack.comm.RandomSelection(selection, randStack.multiplierItemsWeight, True)
            dCost -= randStack.AllItems(r).itemCost.Gold
            Return r
        Else
            'Throw New Exception("Не могу выбрать предмет в качестве товара. Планка цены: " & bar.ToString)
            Return -1
        End If
    End Function
#End Region
#End Region

#Region "Content settings creation"
    Private Function GetSettingsCommon(ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        For Each item In input
            output.Add(item.ToUpper)
        Next item
        Return output
    End Function
    Private Function GetRandomMode(ByRef input As List(Of String), ByRef weight() As Double, ByRef handler As getSettings) As List(Of String)
        Dim output As New List(Of String)
        Dim ID As New List(Of Integer)
        For i As Integer = 0 To UBound(weight) Step 1
            ID.Add(i)
        Next i
        For Each item In input
            Dim r As Integer = randStack.comm.RandomSelection(ID, weight, True) + 1
            output.Add(handler(r, {item}).Item(0))
        Next item
        Return output
    End Function

    Private Function RndPart(ByRef minRange As Integer, ByRef maxRange As Integer, ByRef N As Integer, ByRef partsCount As Integer) As Integer
        Dim d As Double = (maxRange - minRange) / partsCount
        Dim max As Double = minRange + N * d
        Dim min As Double = max - d
        Dim v As Double = randStack.rndgen.Rand(min, max, True)
        Dim p0 As Double = Math.Floor(v)
        Dim p1 As Double = v - p0
        If p1 > 0 Then
            Dim r As Double = randStack.rndgen.Rand(0, 1, True)
            If r <= p1 Then
                p1 = 1
            Else
                p1 = 0
            End If
        End If
        Return CInt(Math.Max(minRange, Math.Min(maxRange, p0 + p1)))
    End Function

#Region "Mage merchant"
    ''' <summary>Вернет настройки генерации заклинаний</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - УровеньРасаМассовость, 3 - Тип, 4 - Тип#УровеньРасаМассовость</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetSpellsListSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)
        Dim L As New List(Of String)
        L.AddRange(input)
        Return GetSpellsListSettings(mode, L)
    End Function
    ''' <summary>Вернет настройки генерации заклинаний</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - УровеньРасаМассовость, 3 - Тип, 4 - Тип#УровеньРасаМассовость</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetSpellsListSettings(ByVal mode As Integer, ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        Dim spell As AllDataStructues.Spell
        If mode = -1 Then
            output = GetRandomMode(input, {0.25, 1, 1, 1}, AddressOf GetSpellsListSettings)
        ElseIf mode = 1 Then
            output = GetSettingsCommon(input)
        Else
            For Each item As String In input
                spell = randStack.FindSpellStats(item)
                If Not spell.spellID = "" Then
                    If randStack.comm.IsPreserved(spell) Then
                        output.Add(spell.spellID.ToUpper)
                    Else
                        Dim category As String = spell.category.ToString
                        Dim prop As String = SpellProperties(spell)
                        If mode = 2 Then
                            output.Add(prop)
                        ElseIf mode = 3 Then
                            output.Add(category)
                        ElseIf mode = 4 Then
                            output.Add(category & "#" & prop)
                        Else
                            Throw New Exception("Unknown mode: " & mode)
                            output = Nothing
                        End If
                    End If
                End If
            Next item
        End If
        Return output
    End Function
    Private Function SpellProperties(ByRef spell As AllDataStructues.Spell) As String
        Dim race As String
        If IsNothing(spell.researchCost) OrElse spell.researchCost.Count = 0 Then
            race = "R"
        Else
            Dim lord As String = spell.researchCost.Keys(0)
            race = randStack.comm.defValues.RaceNumberToRaceChar.Item(randStack.comm.LordsRace.Item(lord))
        End If
        Dim mass As String
        If spell.area > 998 Then
            mass = "T"
        Else
            mass = "F"
        End If
        Return spell.level & race & mass
    End Function
    ''' <summary>Вернет настройки генерации заклинаний</summary>
    ''' <param name="m">Готовая карта</param>
    ''' <param name="sett">Настройки локаций</param>
    ''' <param name="x">Положение по X</param>
    ''' <param name="y">Положение по Y</param>
    ''' <param name="nRaces">Количество играбельных рас</param>
    Public Function GetSpellsListSettings(ByRef m As Map, ByRef sett() As Map.SettingsLoc, ByVal x As Integer, ByVal y As Integer, ByVal nRaces As Integer) As List(Of String)
        Dim s As Map.SettingsLoc = sett(m.board(x, y).locID(0) - 1)
        Dim res As New List(Of String)
        Dim level As Integer
        Dim spellRace, canBeMass As String
        If m.board(x, y).locID(0) > nRaces Then
            spellRace = My.Resources.spellRandomRace
        Else
            spellRace = randStack.comm.defValues.RaceNumberToRaceChar(m.board(x, y).mapObject.objRace(0))
        End If
        For i As Integer = 1 To s.mageSpellsCount Step 1
            level = RndPart(s.mageSpellsMinLevel, s.mageSpellsMaxLevel, i, s.mageSpellsCount)
            If s.mageGlobalSpellsEnabled Then
                If randStack.rndgen.RndInt(0, 1, True) = 1 Then
                    canBeMass = "T"
                Else
                    canBeMass = "F"
                End If
            Else
                canBeMass = "F"
            End If
            res.Add(level & spellRace & canBeMass)
        Next i
        Return res
    End Function
#End Region
#Region "Units merchant"
    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Цена, 3 - Тип, 4 - Тип#Цена</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetMerchantListSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)
        Dim L As New List(Of String)
        L.AddRange(input)
        Return GetMerchantListSettings(mode, L)
    End Function
    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Цена, 3 - Тип, 4 - Тип#Цена</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetMerchantListSettings(ByVal mode As Integer, ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        Dim thing As AllDataStructues.Item
        If mode = -1 Then
            output = GetRandomMode(input, {0.25, 1, 1, 1}, AddressOf GetMerchantListSettings)
        ElseIf mode = 1 Then
            output = GetSettingsCommon(input)
        Else
            For Each item In input
                thing = randStack.FindItemStats(item)
                If Not thing.itemID = "" Then
                    If randStack.comm.IsPreserved(thing) Then
                        output.Add(item.ToUpper)
                    Else
                        Dim cost As String = AllDataStructues.Cost.Sum(thing.itemCost).ToString
                        Dim type As String = randStack.comm.itemType.Item(thing.type)
                        If mode = 2 Then
                            output.Add(cost)
                        ElseIf mode = 3 Then
                            output.Add(type)
                        ElseIf mode = 4 Then
                            output.Add(type & "#" & cost)
                        Else
                            Throw New Exception("Unknown mode: " & mode)
                            output = Nothing
                        End If
                    End If
                End If
            Next item
        End If
        Return output
    End Function
    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="m">Готовая карта</param>
    ''' <param name="sett">Настройки локаций</param>
    ''' <param name="x">Положение по X</param>
    ''' <param name="y">Положение по Y</param>
    Public Function GetMerchantListSettings(ByRef m As Map, ByRef sett() As Map.SettingsLoc, ByVal x As Integer, ByVal y As Integer) As List(Of String)
        Dim s As Map.SettingsLoc = sett(m.board(x, y).locID(0) - 1)
        Dim res As New List(Of String)
        Dim cost As Integer = s.merchItemsCost
        Dim selected, itemcost, minCost, maxCost As Integer
        Dim addOnce, addedTypes, selection, exclude As New List(Of Integer)
        addOnce.AddRange(New Integer() {GenDefaultValues.ItemTypes.attack_artifact, GenDefaultValues.ItemTypes.nonattack_artifact, _
                                        GenDefaultValues.ItemTypes.banner, GenDefaultValues.ItemTypes.boots, GenDefaultValues.ItemTypes.permanent_elixir, _
                                        GenDefaultValues.ItemTypes.relic, GenDefaultValues.ItemTypes.stuff, GenDefaultValues.ItemTypes.talisman})
        For Each item As Integer In excludedItemType
            exclude.Add(item)
        Next item

        For Each v As GenDefaultValues.ItemTypes In System.Enum.GetValues(GetType(GenDefaultValues.ItemTypes))
            If randStack.comm.IsExcluded(v) AndAlso Not exclude.Contains(v) Then exclude.Add(v)
        Next v
        For Each deltaCost As Integer In {400, 200, 200, 100}
            selected = GenDefaultValues.ItemTypes.healing_elixir
            Dim c As Integer = Math.Min(typeMaxCost(selected), Math.Max(typeMinCost(selected), deltaCost))
            cost -= c
            res.Add(randStack.comm.itemType.Item(selected) & "#" & c)
            If Not addedTypes.Contains(selected) Then addedTypes.Add(selected)
        Next deltaCost
        For Each deltaCost As Integer In {400, 400}
            selected = GenDefaultValues.ItemTypes.ressurection_elixir
            Dim c As Integer = Math.Min(typeMaxCost(selected), Math.Max(typeMinCost(selected), deltaCost))
            cost -= c
            res.Add(randStack.comm.itemType.Item(selected) & "#" & c)
            If Not addedTypes.Contains(selected) Then addedTypes.Add(selected)
        Next deltaCost
        Do While cost > 0
            selection.Clear()
            For Each v As Integer In System.Enum.GetValues(GetType(GenDefaultValues.ItemTypes))
                If Not exclude.Contains(v) Then
                    If Not addOnce.Contains(v) OrElse Not addedTypes.Contains(v) Then selection.Add(v)
                End If
            Next v
            selected = randStack.comm.RandomSelection(selection, True)
            If randStack.comm.ConsumableItemsTypes.Contains(selected) Then
                minCost = s.merchMinConsumableItemCost
                maxCost = s.merchMaxConsumableItemCost
            Else
                minCost = s.merchMinNonconsumableItemCost
                maxCost = s.merchMaxNonconsumableItemCost
            End If
            If cost > minCost Then
                itemcost = randStack.rndgen.RndInt(minCost, maxCost, True)
            Else
                itemcost = cost
            End If
            itemcost = Math.Min(typeMaxCost(selected), Math.Max(typeMinCost(selected), itemcost))
            cost -= itemcost
            res.Add(randStack.comm.itemType.Item(selected) & "#" & itemcost)
            If Not addedTypes.Contains(selected) Then addedTypes.Add(selected)
        Loop
        Return res
    End Function
#End Region
#Region "Units merchant"
    ''' <summary>Вернет настройки генерации наемников</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Планка опыта, 3 - Раса, 4 - Раса#Планка опыта</param>
    ''' <param name="input">ID юнитов</param>
    Public Function GetMercenariesListSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)
        Dim L As New List(Of String)
        L.AddRange(input)
        Return GetMercenariesListSettings(mode, L)
    End Function
    ''' <summary>Вернет настройки генерации наемников</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Планка опыта, 3 - Раса, 4 - Раса#Планка опыта</param>
    ''' <param name="input">ID юнитов</param>
    Public Function GetMercenariesListSettings(ByVal mode As Integer, ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        Dim unit As AllDataStructues.Unit
        If mode = -1 Then
            output = GetRandomMode(input, {0.25, 1, 1, 1}, AddressOf GetMercenariesListSettings)
        ElseIf mode = 1 Then
            output = GetSettingsCommon(input)
        Else
            For Each item In input
                unit = randStack.FindUnitStats(item)
                If Not unit.unitID = "" Then
                    If randStack.comm.IsPreserved(unit) Then
                        output.Add(item.ToUpper)
                    Else
                        Dim expNext As Integer = unit.EXPnext
                        If Not unit.small Then expNext = CInt(expNext * 0.5)
                        Dim race As String = randStack.comm.defValues.RaceNumberToRaceChar(unit.race)
                        If mode = 2 Then
                            output.Add(expNext.ToString)
                        ElseIf mode = 3 Then
                            output.Add(race)
                        ElseIf mode = 4 Then
                            output.Add(race & "#" & expNext.ToString)
                        Else
                            Throw New Exception("Unknown mode: " & mode)
                            output = Nothing
                        End If
                    End If
                End If
            Next item
        End If
        Return output
    End Function
    ''' <summary>Вернет настройки генерации наемников</summary>
    ''' <param name="m">Готовая карта</param>
    ''' <param name="sett">Настройки локаций</param>
    ''' <param name="x">Положение по X</param>
    ''' <param name="y">Положение по Y</param>
    ''' <param name="nRaces">Количество играбельных рас</param>
    Public Function GetMercenariesListSettings(ByRef m As Map, ByRef sett() As Map.SettingsLoc, ByVal x As Integer, ByVal y As Integer, ByVal nRaces As Integer) As List(Of String)
        Dim s As Map.SettingsLoc = sett(m.board(x, y).locID(0) - 1)
        Dim res As New List(Of String)
        Dim mercRace As String
        Dim selected As String
        Dim raceID As Integer
        Dim bar As Integer
        For i As Integer = 1 To s.mercenariesCount Step 1
            If m.board(x, y).locID(0) > nRaces Then
                selected = randStack.comm.defValues.neutralRaces(randStack.rndgen.RndInt(0, UBound(randStack.comm.defValues.neutralRaces), True))
                raceID = randStack.comm.RaceIdentifierToSubrace(selected)
            Else
                raceID = m.board(x, y).mapObject.objRace(0)
            End If
            mercRace = randStack.comm.defValues.RaceNumberToRaceChar(raceID)
            bar = RndPart(s.mercenariesMinExpBar, s.mercenariesMaxExpBar, i, s.mercenariesCount)
            res.Add(mercRace & "#" & bar)
        Next i
        Return res
    End Function
#End Region
#End Region

#Region "Lord creation"
    ''' <summary>Вернет ID случайного лорда, соответствующего расе</summary>
    ''' <param name="race">Раса</param>
    ''' <param name="isGameRace">True - раса в виде G000RR0000, False - раса обозначена символом, используемым генератором</param>
    Public Function LordRandomizer(ByRef race As String, ByRef isGameRace As Boolean) As String
        Dim r As Integer
        If isGameRace Then
            r = randStack.comm.RaceIdentifierToSubrace(randStack.comm.defValues.gameRaceToGeneratorRace.Item(race.ToUpper))
        Else
            r = randStack.comm.RaceIdentifierToSubrace(race.ToUpper)
        End If
        Dim ids As New List(Of Integer)
        Dim lords(randStack.comm.LordsRace.Count - 1) As String
        Dim n As Integer = -1
        For Each lord As String In randStack.comm.LordsRace.Keys
            n += 1
            lords(n) = lord
            If randStack.comm.LordsRace.Item(lord) = r Then ids.Add(n)
        Next lord
        If ids.Count = 0 Then Throw New Exception("Нет ни одного лорда, соответствующего заданной расе")
        Dim selected As Integer = randStack.comm.RandomSelection(ids, True)
        Return lords(selected)
    End Function
#End Region
End Class

Public Class VanillaSagaContentReplace

    Dim RndStack As RandStack

    Dim VanillaLoreUnits As List(Of String)

    ''' <param name="RStack">Инициализированный класс</param>
    Public Sub New(ByRef RStack As RandStack)
        RndStack = RStack
        'RStack.comm.ReadExcludedObjectsList(New String() {".\Resources\mod_settings_Vanilla\ExcludeIDs_ModLore.txt"})
        'VanillaLoreUnits = RStack.comm.excludedObjects
        MsgBox("Если вдруг класс понадобится, нужно сделать чтение файла с лорными юнитами ванили")
    End Sub

    Public Function ReplaceItem(ByRef ID As String) As String

        Dim item As AllDataStructues.Item = RndStack.FindItemStats(ID)

        If item.type = GenDefaultValues.ItemTypes.special Then Return ID

        Dim selection As New List(Of Integer)
        Dim dtolerance As Integer = 100
        Dim tolerance As Integer = 0
        Dim oneMore As Boolean = False
        Dim add As Boolean

        Do While (selection.Count = 0 Or oneMore) And tolerance <= 10000
            tolerance += dtolerance
            selection.Clear()
            For u As Integer = 0 To UBound(RndStack.AllItems) Step 1
                If RndStack.comm.IsAppropriateItem(RndStack.AllItems(u)) Then
                    If Math.Abs(RndStack.LootCost(RndStack.AllItems(u)).Gold - RndStack.LootCost(item).Gold) <= tolerance Then
                        If item.type = RndStack.AllItems(u).type Then
                            add = True
                        Else
                            add = False
                        End If
                        If add Then selection.Add(u)
                    End If
                End If
            Next u
            If selection.Count > 0 Then oneMore = Not oneMore
        Loop
        If selection.Count > 0 Then
            Dim r As Integer = RndStack.comm.RandomSelection(selection, True)
            Return RndStack.AllItems(r).itemID
        Else
            Return ID
        End If
    End Function

    Private Delegate Function UnitFilter(ByRef unit As AllDataStructues.Unit) As Boolean
    Public Function ReplaceUnit(ByRef ID As String) As String

        Dim unit As AllDataStructues.Unit = RndStack.FindUnitStats(ID)

        If unit.unitBranch = GenDefaultValues.UnitClass.capitalGuard Then Return unit.unitID
        If VanillaLoreUnits.Contains(unit.unitID) Then Return unit.unitID

        Dim selection As New List(Of Integer)
        Dim dtolerance As Integer = 100
        Dim tolerance As Integer = 0
        Dim oneMore As Boolean = False
        Dim add As Boolean

        Dim f As UnitFilter
        If unit.unitBranch = GenDefaultValues.UnitClass.leader Then
            f = AddressOf RndStack.comm.IsAppropriateLeader
        Else
            f = AddressOf RndStack.comm.IsAppropriateFighter
        End If

        Do While (selection.Count = 0 Or oneMore) And tolerance <= 10000
            tolerance += dtolerance
            selection.Clear()
            For u As Integer = 0 To UBound(RndStack.AllUnits) Step 1
                If f(RndStack.AllUnits(u)) Then
                    If Math.Abs(RndStack.AllUnits(u).EXPkilled - unit.EXPkilled) <= tolerance Then
                        add = True
                        If add Then add = (RndStack.AllUnits(u).small = unit.small)
                        If add Then add = (RndStack.AllUnits(u).waterOnly = unit.waterOnly)
                        If add Then add = (RndStack.AllUnits(u).leadership = unit.leadership)
                        If add Then
                            If RndStack.AllUnits(u).reach = GenDefaultValues.UnitAttackReach.melee Then
                                add = (RndStack.AllUnits(u).reach = unit.reach)
                            Else
                                add = Not (RndStack.AllUnits(u).reach = GenDefaultValues.UnitAttackReach.melee)
                            End If
                        End If
                        If add Then add = (RndStack.AllUnits(u).race = unit.race)
                        If add Then selection.Add(u)
                    End If
                End If
            Next u
            If selection.Count > 0 Then oneMore = Not oneMore
        Loop
        If selection.Count > 0 Then
            Dim r As Integer = RndStack.comm.RandomSelection(selection, True)
            Return RndStack.AllUnits(r).unitID
        Else
            Return unit.unitID
        End If
    End Function

End Class
