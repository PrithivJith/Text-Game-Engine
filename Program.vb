

'By Prithiv 10E
Imports System.Text.Json


Module Program
    Sub Debug(text)
        Console.WriteLine($"{text}:{text.GetType()}")
        Console.ReadLine()
    End Sub

    Sub Draw(img As String, x As Integer, y As Integer)
        Dim imglines() As String = img.Split(vbCrLf)
        For Each line As String In imglines
            Console.SetCursorPosition(x, y)
            Console.Write(line)
            y += 1
        Next
    End Sub
    Class States
        Public Property Inventory As List(Of String)
        Public Property Visited As Dictionary(Of String, Integer)
        Public Property states As Dictionary(Of String, List(Of Object))
        Public Sub New()
            Visited = New Dictionary(Of String, Integer)
        End Sub

        Public Sub MarkScene(sceneName As String)
            If Visited.ContainsKey(sceneName) Then
                Visited(sceneName) += 1
            Else
                Visited(sceneName) = 1
            End If
        End Sub


        Public Function VisitedTimes(sceneName As String) As Integer
            If Visited.ContainsKey(sceneName) Then
                Return Visited(sceneName)
            Else
                Return 0
            End If
        End Function
    End Class
    Class Choice
        Public Property Text As String
        Public Property NextScene As String
        Public Property Condition As BuildCondition

        Public ConditionFunc As Func(Of States, Boolean)
    End Class



    Class Scene
        Public Property Name As String
        Public Property Description As String
        Public Property Choices As List(Of Choice)
        Public Property stateChange As Dictionary(Of String, JsonElement)
        Public Property InventoryAdd As List(Of String)

    End Class

    Function JsonToScenes(jsonText As String) As Dictionary(Of String, Scene)
        Dim options As New System.Text.Json.JsonSerializerOptions With {.PropertyNameCaseInsensitive = True}
        Dim scenes As List(Of Scene) = System.Text.Json.JsonSerializer.Deserialize(Of List(Of Scene))(jsonText, options)

        Dim sceneDict As Dictionary(Of String, Scene) = scenes.ToDictionary(Function(s) s.Name, Function(s) s)
        Return sceneDict
    End Function






    Function GetChoice(scene As Scene, state As States, initialStates As Dictionary(Of String, List(Of Object))) As String

        Dim validChoices = scene.Choices.
                Where(Function(c) c.Condition Is Nothing OrElse c.ConditionFunc(state)).ToList()
        If validChoices.Count = 0 Then
            Console.Clear()
            'dipslay stats
            displayProp(state, initialStates)

            Console.WriteLine(New String("-", Console.WindowWidth))
            Type(interpolateStates(state, scene.Description))
            Console.WriteLine()
            Console.ReadLine()
            Return "__end__"
        End If
        Dim selection As Integer = 1
        Dim maxLength As Integer = validChoices.Max(Function(c) c.Text.Length + 3)
        Dim distance As Integer = maxLength + 4
        Dim savex, savey As Integer
        savex = Console.CursorLeft
        savey = Console.CursorTop
        Dim y As Integer
        Dim keyChar As ConsoleKey

        Dim first As Boolean = True
        While True
            Console.Clear()
            'dipslay stats
            displayProp(state, initialStates)
            Console.WriteLine(New String("-", Console.WindowWidth))
            If first And state.VisitedTimes(scene.Name) = 1 Then
                Type(interpolateStates(state, scene.Description))
                first = False
            Else
                Type(interpolateStates(state, scene.Description), 0)
            End If
            Console.WriteLine()

            For i = 0 To validChoices.Count - 1
                Console.Write($"{i + 1}. {interpolateStates(state, validChoices(i).Text)}")
                y = Console.CursorTop
                Console.SetCursorPosition(distance, y)
                If selection - 1 = i Then
                    Console.WriteLine("<")
                Else
                    Console.WriteLine(" ")
                End If
            Next
            keyChar = Console.ReadKey(True).Key
            Select Case keyChar
                Case ConsoleKey.UpArrow
                    If selection > 1 Then
                        selection -= 1
                    End If
                Case ConsoleKey.DownArrow
                    If selection < validChoices.Count Then
                        selection += 1
                    End If
                Case Else
                    If keyChar = ConsoleKey.Enter Or keyChar = ConsoleKey.Spacebar Then
                        Exit While
                    End If
            End Select
        End While
        'By Prithiv 10E


        Return validChoices(selection - 1).NextScene
    End Function

    Sub UpdateStates(state As States, scene As Scene)
        Dim positiveKeys As List(Of String) = state.states("Positive")(0).ToString().Split(";"c).ToList()

        If scene.stateChange IsNot Nothing Then
            For Each kvp In scene.stateChange

                Dim stateChangeKey = kvp.Key
                Dim stateChangeValue As Object = kvp.Value

                If stateChangeValue.ValueKind = JsonValueKind.String Then
                    stateChangeValue = stateChangeValue.GetString()
                Else
                    stateChangeValue = stateChangeValue.GetInt32()
                End If

                Dim stateChangeInterpolated As Object = stateChangeValue
                Dim operation As String = ""

                If TypeOf stateChangeValue Is String Then
                    Dim s As String = CStr(stateChangeValue)
                    Dim parts() As String = s.Split(";"c)

                    operation = parts(0).Trim()
                    Dim right As String = parts(1).Trim()
                    If right.Length > 2 AndAlso right(0) = "{"c AndAlso right(right.Length - 1) = "}"c Then
                        Dim interpolateValKey As String = right.Substring(1, right.Length - 2)
                        If state.states.ContainsKey(interpolateValKey) Then
                            stateChangeInterpolated = state.states(interpolateValKey)(0)
                        End If
                    Else
                        stateChangeInterpolated = right
                    End If


                    If IsNumeric(stateChangeInterpolated) Then
                        stateChangeInterpolated = CInt(stateChangeInterpolated)
                    End If
                End If


                If state.states.ContainsKey(stateChangeKey) Then
                    If TypeOf stateChangeInterpolated Is Integer Then
                        Select Case operation
                            Case "="
                                state.states(stateChangeKey)(0) = stateChangeInterpolated
                            Case "-"
                                state.states(stateChangeKey)(0) -= stateChangeInterpolated
                            Case "+"
                                state.states(stateChangeKey)(0) += stateChangeInterpolated
                            Case "*"
                                state.states(stateChangeKey)(0) *= stateChangeInterpolated
                        End Select

                        If positiveKeys.Contains(stateChangeKey) AndAlso CInt(state.states(stateChangeKey)(0)) < 0 Then
                            state.states(stateChangeKey)(0) = 0
                        End If
                    ElseIf TypeOf stateChangeInterpolated Is String Then
                        Select Case operation
                            Case "="
                                state.states(stateChangeKey)(0) = stateChangeInterpolated
                            Case "+"
                                state.states(stateChangeKey)(0) &= stateChangeInterpolated
                        End Select
                    End If
                End If
            Next
        End If


        If scene.InventoryAdd IsNot Nothing AndAlso scene.InventoryAdd.Count > 0 Then
            Dim operation As Char = scene.InventoryAdd(0)(0)
            Select Case operation
                Case "0"c
                    state.Inventory = New List(Of String)
                Case "+"c
                    For i As Integer = 1 To scene.InventoryAdd.Count - 1
                        Dim item As String = scene.InventoryAdd(i)
                        If Not state.Inventory.Contains(item) Then
                            state.Inventory.Add(item)
                        End If
                    Next
                Case "-"c
                    For i As Integer = 1 To scene.InventoryAdd.Count - 1
                        Dim item As String = scene.InventoryAdd(i)
                        If state.Inventory.Contains(item) Then
                            state.Inventory.Remove(item)
                        End If
                    Next
            End Select
        End If
    End Sub
    Class BuildCondition
        Public Property State As String
        Public Property Op As String
        Public Property Value As String
        Public Property Scene As String
        Public Property [Not] As Boolean

        Public Property All As List(Of BuildCondition)
        Public Property Any As List(Of BuildCondition)

        Sub New()
            [Not] = False
            Scene = ""
        End Sub
        Function BuildFunction() As Func(Of States, Boolean)
            'By Prithiv 10E

            If All IsNot Nothing AndAlso All.Count > 0 Then
                Dim funcs = All.Select(Function(c) c.BuildFunction()).ToList()
                Return Function(s As States)
                           Return funcs.All(Function(f) f(s))
                       End Function
            End If

            If Any IsNot Nothing AndAlso Any.Count > 0 Then
                Dim funcs = Any.Select(Function(c) c.BuildFunction()).ToList()
                Return Function(s As States)
                           Return funcs.Any(Function(f) f(s))
                       End Function
            End If
            Return CType(Function(s As States) As Boolean

                             If String.IsNullOrEmpty(State) Then
                                 Return True
                             End If

                             Dim result As Boolean

                             If State = "Visited" AndAlso Scene <> "" Then
                                 Select Case Op
                                     Case "="
                                         result = s.VisitedTimes(Scene) = CInt(Value)
                                     Case "<>"
                                         result = s.VisitedTimes(Scene) <> CInt(Value)
                                     Case "<"
                                         result = s.VisitedTimes(Scene) < CInt(Value)
                                     Case ">"
                                         result = s.VisitedTimes(Scene) > CInt(Value)
                                 End Select
                             ElseIf State = "Inventory" Then
                                 If Op = "=" Then
                                     result = s.Inventory.Contains(CStr(Value))

                                 ElseIf Op = "<>" Then
                                     result = Not s.Inventory.Contains(CStr(Value))
                                 End If
                             Else
                                 If TypeOf s.states(State)(0) Is Integer Then
                                     Select Case Op
                                         Case "="
                                             result = CInt(s.states(State)(0)) = CInt(Value)
                                         Case "<>"
                                             result = CInt(s.states(State)(0)) <> CInt(Value)
                                         Case "<"
                                             result = CInt(s.states(State)(0)) < CInt(Value)
                                         Case ">"
                                             result = CInt(s.states(State)(0)) > CInt(Value)
                                     End Select
                                 ElseIf TypeOf s.states(State)(0) Is String Then
                                     If Op = "=" Then
                                         result = CStr(s.states(State)(0)) = CStr(Value)
                                     ElseIf Op = "<>" Then
                                         result = CStr(s.states(State)(0)) <> CStr(Value)
                                     End If
                                 Else
                                     If TypeOf s.states(State)(0) Is Integer Then
                                         Select Case Op
                                             Case "="
                                                 result = CInt(s.states(State)(0)) = CInt(Value)
                                             Case "<>"
                                                 result = CInt(s.states(State)(0)) <> CInt(Value)
                                             Case "<"
                                                 result = CInt(s.states(State)(0)) < CInt(Value)
                                             Case ">"
                                                 result = CInt(s.states(State)(0)) > CInt(Value)
                                         End Select
                                     ElseIf TypeOf s.states(State)(0) Is String Then
                                         If Op = "=" Then
                                             result = CStr(s.states(State)(0)) = CStr(Value)
                                         ElseIf Op = "<>" Then
                                             result = CStr(s.states(State)(0)) <> CStr(Value)
                                         End If

                                     End If
                                 End If
                             End If
                             If [Not] Then
                                 Return Not result
                             Else
                                 Return result
                             End If
                         End Function, Func(Of States, Boolean))
        End Function
    End Class

    Function interpolateStates(s As States, text As String) As String
        Dim result As String = ""
        Dim buffer As String = ""
        Dim inside As Boolean = False

        For Each c As Char In text
            If c = "{"c Then
                buffer = ""
                inside = True
            ElseIf c = "}"c AndAlso inside Then
                If s.states.ContainsKey(buffer) Then
                    result &= s.states(buffer)(0).ToString()
                Else
                    result &= $"{{{buffer}}}"
                End If
                inside = False
            ElseIf inside Then
                buffer &= c
            Else
                result &= c
            End If
        Next
        Return result
    End Function

    Sub convertStates(state As States)
        For Each p In state.states.ToList()
            Dim value As JsonElement = p.Value(0)
            If value.ValueKind = JsonValueKind.Number Then
                state.states(p.Key)(0) = value.GetInt32()
            ElseIf value.ValueKind = JsonValueKind.String Then
                state.states(p.Key)(0) = value.ToString()
            End If
        Next
        'By Prithiv 10E

    End Sub
    Function CapitalizeFirst(text As String) As String
        If String.IsNullOrEmpty(text) Then Return text
        Return Char.ToUpper(text(0)) & text.Substring(1).ToLower()
    End Function
    Sub Type(t As Object, Optional delay As Integer = 1)
        Dim text As String = CStr(t)
        For i = 0 To text.Count - 1
            If Console.KeyAvailable Then
                Dim key As ConsoleKeyInfo = Console.ReadKey(intercept:=True)
                If key.Key = ConsoleKey.Spacebar Or key.Key = ConsoleKey.Enter Then
                    delay = 0
                End If
            End If

            Console.Write(text(i))
            System.Threading.Thread.Sleep(delay)
        Next
        Console.WriteLine()
    End Sub
    Sub displayProp(state As States, initialState As Dictionary(Of String, List(Of Object)))
        Dim fullBar As Char = "▓"c
        Dim emptyBar As Char = "▒"
        Dim colors As New Dictionary(Of String, ConsoleColor) From {
    {"black", 0},
    {"darkblue", 1},
    {"darkgreen", 2},
    {"darkcyan", 3},
    {"darkred", 4},
    {"darkmagenta", 5},
    {"darkyellow", 6},
    {"gray", 7},
    {"darkgray", 8},
    {"blue", 9},
    {"green", 10},
    {"cyan", 11},
    {"red", 12},
    {"magenta", 13},
    {"yellow", 14},
    {"white", 15}}
        Dim fillCol As ConsoleColor
        Dim emptyCol As ConsoleColor
        Dim x, y As Integer
        Dim barCount As Integer = 0
        x = Console.WindowWidth - 3
        y = -1
        Dim barLen As Integer = 30
        Dim textParts As New List(Of String)
        For Each s In state.states
            If s.Value(1).ToString() = "text" Then
                textParts.Add($"{CapitalizeFirst(s.Key)}: {s.Value(0)}")
            End If
        Next
        For Each s In state.states
            If s.Value(1).ToString().Split(";").Count > 1 AndAlso s.Value(1).ToString().Split(";")(0) = "bar" Then
                fillCol = colors(s.Value(1).ToString().Split(";")(1))
                emptyCol = colors(s.Value(1).ToString().Split(";")(2))

                barCount += 1
                Dim currentVal As Integer = s.Value(0).ToString()
                Dim maxVal As Integer = initialState(s.Key)(0).ToString()
                Dim pct As Double = Math.Min(Math.Max(currentVal / maxVal, 0), 1)
                Dim filled As New String(fullBar, CInt(pct * barLen))
                Dim empty As New String(emptyBar, barLen - CInt(pct * barLen))
                Dim dispx As Integer = -($"{filled}{empty}{New String("", (3 + (2 * 4)) - $"({currentVal}/{maxVal})".Count)}({currentVal}/{maxVal})".Length)
                Console.ForegroundColor = ConsoleColor.White
                Draw($"{CapitalizeFirst(s.Key)}: ", x + dispx - $"{CapitalizeFirst(s.Key)}: ".Length, y + barCount)
                Console.ForegroundColor = fillCol
                Draw(filled, x + dispx, y + barCount)
                Console.ForegroundColor = emptyCol
                Draw(empty, filled.Length + x + dispx, y + barCount)
                Console.ForegroundColor = ConsoleColor.White
                Console.Write($"   ({currentVal}/{maxVal})")
                Console.SetCursorPosition(0, 0)

            End If
        Next
        Console.WriteLine(String.Join(" | ", textParts))
        For i = 0 To barCount - 1
            Console.WriteLine()
        Next
        Console.WriteLine($"Inventory: {String.Join(", ", state.Inventory)}")
        'Console.WriteLine()
    End Sub


    Function input(text As String)
        'By Prithiv 10E

        Console.Write(text)
        Return (Console.ReadLine())
    End Function
    Function checkCritical(state As States)
        If state.states("Critical")(0).ToString().Split(";")(0) = "" Then
            Return False
        End If
        For Each s In state.states("Critical")(0).ToString().Split(";")

            If CInt(state.states(s)(0).ToString()) < 1 Then
                Console.Clear()
                Console.Write($"You ran out of {s}!")
                Console.ReadLine()
                Return True
            End If
        Next
        Return False
    End Function

    Sub Main()
        Console.Clear()
        Console.CursorVisible = False
        Console.WriteLine("By Prithiv 10E")
        Dim userLoadChoice As String = input("Name/Num: ")
        Dim json As String = ""
        If Not IsNumeric(userLoadChoice) Then
            json = System.IO.File.ReadAllText(userLoadChoice)
        Else
            json = System.IO.File.ReadAllText($"story{userLoadChoice}.json")
        End If
        Dim doc As JsonDocument = JsonDocument.Parse(json)
        Dim root = doc.RootElement
        Dim initialStates As Dictionary(Of String, List(Of Object)) = JsonSerializer.Deserialize(Of Dictionary(Of String, List(Of Object)))(root.GetProperty("initialStates").GetRawText())
        Dim running As Boolean = True


        Dim state As New States With {
            .Inventory = New List(Of String),
            .states = initialStates.ToDictionary(Function(kv) kv.Key, Function(kv) kv.Value.ToList())
        }
        convertStates(state)

        Dim scenes As Dictionary(Of String, Scene) = JsonToScenes(root.GetProperty("scenes").GetRawText())
        For Each sc In scenes.Values
            For Each ch In sc.Choices
                If ch.Condition IsNot Nothing Then
                    ch.ConditionFunc = CType(ch.Condition.BuildFunction(), Func(Of States, Boolean))
                End If
            Next
        Next


        Dim currentSceneText As String = state.states("Start")(0).ToString()
        Dim currentScene As Scene

        While running
            If checkCritical(state) Then
                Exit While
            End If
            currentScene = scenes(currentSceneText)
            state.MarkScene(currentSceneText)

            UpdateStates(state, currentScene)

            Console.Clear()

            currentSceneText = GetChoice(currentScene, state, initialStates)
            If checkCritical(state) Or currentSceneText = "__end__" Then
                Exit While
            End If
        End While
        Main()
    End Sub




End Module
