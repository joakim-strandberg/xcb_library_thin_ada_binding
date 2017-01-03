with Ada.Characters.Latin_1;
--  with Ada.Text_IO;
with Aida;

procedure Aida.XML.Parse_XML_File (Contents      : Standard.String;
                                   Error_Message : out Error_Message_P.T;
                                   Is_Success    : out Boolean)
is
   use type Aida.Code_Point_T;

   use Aida.XML.Bounded_String;
   use Aida.XML.DL;
   use Error_Message_P;

   Tag_Names : DL.T;

   type State_Id_Type is (
                          Searching_For_XML_Start_String,
                          Expecting_NL_Sign_Or_Space_Or_Less_Sign, -- NL = New Line
                          Found_Less_Sign,
                          Found_Less_Followed_By_Exclamation_Sign,
                          Found_Less_Followed_By_Exclamation_And_Dash_Sign,
                          Ignore_Comment,
                          Ignore_Comment_Followed_By_Dash_Sign,
                          Ignore_Comment_Followed_By_Two_Dash_Signs,
                          Extracting_Start_Tag_Name,
                          Expecting_G_Sign_Or_Extracting_Attributes,
                          Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash,
                          Extracting_Attribute_Name,
                          Expecting_Attribute_Value_Quotation_Mark,
                          Extracting_Attribute_Value,
                          Expecting_New_Tag_Or_Extracting_Tag_Value,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L,
                          Extracting_End_Tag_Name,
                          -- Enumeration values introduced to handle <!CDATA[--]]>
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket,
                          Extracting_CDATA,
                          Extracting_CDATA_Found_Square_Bracket,
                          Extracting_CDATA_Found_Two_Square_Brackets,
                          Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign,
                          Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign
                         );

   State_Id : State_Id_Type := Searching_For_XML_Start_String;

   XML_File_Start_String : constant Standard.String := "<?xml version=""1.0"" encoding=""utf-8""?>";

   F : constant Natural := Contents'First;

   package Boolean_List is new Aida.Containers.Bounded_Vector (Element_T  => Boolean,
                                                               "="        => "=",
                                                               MAX_LENGTH => 20);

   use Boolean_List;

   Shall_Ignore_Tag_Value_List : Boolean_List.T;

   function Is_Special_Symbol (CP : Aida.Code_Point_T) return Boolean is
   begin
      if CP = Standard.Character'Pos ('<') then
         return True;
      elsif CP = Standard.Character'Pos ('>') then
         return True;
      elsif CP = Standard.Character'Pos ('/') then
         return True;
      elsif CP = Standard.Character'Pos ('"') then
         return True;
      else
         return False;
      end if;
   end Is_Special_Symbol;
begin
   if Contents (F..F+XML_File_Start_String'Length-1) /= XML_File_Start_String then
      Initialize (Error_Message, "File does not start with <?xml version...>");
      Is_Success := False;
      return;
   end if;

   Clear (Tag_Names);

   State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

   declare
      P           : Integer := Aida.UTF8.Length (XML_File_Start_String) + 1;
      Prev_P      : Integer := P;
      Prev_Prev_P : Integer := Prev_P;

      CP      : Aida.Code_Point_T := 0;

      Start_Tag_Name_First_Index : Integer := -1;
      Start_Tag_Name_Last_Index  : Integer := -1;

      Tag_Value_First_Index : Integer := -1;
      Tag_Value_Last_Index  : Integer := -1;

      End_Tag_Name_First_Index : Integer := -1;
      End_Tag_Name_Last_Index  : Integer := -1;

      Attribute_First_Index : Integer := -1;
      Attribute_Last_Index  : Integer := -1;

      Attribute_Value_First_Index : Integer := -1;
      Attribute_Value_Last_Index  : Integer := -1;

      Shall_Ignore_Tag_Value : Boolean := False;
   begin
      while P < Contents'Last loop
         Prev_Prev_P := Prev_P;

         Prev_P := P;

         Aida.UTF8.Get (Source  => Contents,
                        Pointer => P,
                        Value   => CP);

--                     Ada.Text_IO.Put_Line ("Extracted:" & Strings_Edit.UTF8.Image (CP) & ", state " & State_Id'Img);
--                     Ada.Text_IO.Put (Strings_Edit.UTF8.Image (CP));

         case State_Id is
            when Searching_For_XML_Start_String =>
               Initialize (Error_Message, "It should not be possible to be in the state " & State_Id'Img);
               Is_Success := False;
               return;
            when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
               if CP = Standard.Character'Pos (Ada.Characters.Latin_1.LF) or CP = Standard.Character'Pos (' ') then
                  null; -- Normal
               elsif CP = Standard.Character'Pos ('<') then
                  State_Id := Found_Less_Sign;
               else
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Found_Less_Sign =>
               if CP = Standard.Character'Pos ('!') then
                  State_Id := Found_Less_Followed_By_Exclamation_Sign;
               elsif CP = Standard.Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               elsif not Is_Special_Symbol (CP) then
                  State_Id := Extracting_Start_Tag_Name;
                  Start_Tag_Name_First_Index := Prev_P;
               else
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Found_Less_Followed_By_Exclamation_Sign =>
               if CP = Standard.Character'Pos ('-') then
                  State_Id := Found_Less_Followed_By_Exclamation_And_Dash_Sign;
               else
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
               if CP = Standard.Character'Pos ('-') then
                  State_Id := Ignore_Comment;
               else
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Ignore_Comment =>
               if CP = Standard.Character'Pos ('-') then
                  State_Id := Ignore_Comment_Followed_By_Dash_Sign;
               end if;
            when Ignore_Comment_Followed_By_Dash_Sign =>
               if CP = Standard.Character'Pos ('-') then
                  State_Id := Ignore_Comment_Followed_By_Two_Dash_Signs;
               else
                  State_Id := Ignore_Comment;
               end if;
            when Ignore_Comment_Followed_By_Two_Dash_Signs =>
               if CP = Standard.Character'Pos ('>') then
                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
               else
                  State_Id := Ignore_Comment;
               end if;
            when Extracting_Start_Tag_Name =>
               if CP = Standard.Character'Pos (' ') then
                  Start_Tag_Name_Last_Index := Prev_Prev_P;
                  Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                             Tag_Names,
                             Error_Message,
                             Is_Success);

                  if not Is_Success then
                     return;
                  end if;

                  declare
                     S : Aida.XML.Bounded_String_T;
                  begin
                     Initialize (This => S,
                                 Text => Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index));
                     Append (This     => Tag_Names,
                             New_Item => S);
                  end;

                  Append (This     => Shall_Ignore_Tag_Value_List,
                          New_Item => Shall_Ignore_Tag_Value);

                  Shall_Ignore_Tag_Value := False;

                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
               elsif CP = Standard.Character'Pos ('>') then
                  Start_Tag_Name_Last_Index := Prev_Prev_P;
                  Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                             Tag_Names,
                             Error_Message,
                             Is_Success);

                  if not Is_Success then
                     return;
                  end if;

                  declare
                     S : Aida.XML.Bounded_String_T;
                  begin
                     Initialize (This => S,
                                 Text => Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index));
                     Append (This     => Tag_Names,
                             New_Item => S);
                  end;

                  Append (This     => Shall_Ignore_Tag_Value_List,
                          New_Item => Shall_Ignore_Tag_Value);

                  Shall_Ignore_Tag_Value := False;
                  Tag_Value_First_Index := P;

                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               elsif Is_Special_Symbol (CP) then
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Expecting_G_Sign_Or_Extracting_Attributes =>
               if CP = Standard.Character'Pos (' ') or CP = Standard.Character'Pos (Ada.Characters.Latin_1.LF) then
                  null; -- Normal
               elsif CP = Standard.Character'Pos ('>') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  Tag_Value_First_Index := P;
               elsif CP = Standard.Character'Pos ('/') then
                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
               elsif not Is_Special_Symbol (CP) then
                  Attribute_First_Index := Prev_P;
                  State_Id := Extracting_Attribute_Name;
               end if;
            when Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash =>
               if CP = Standard.Character'Pos ('>') then
                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

                  declare
                     Tag_Name : constant Standard.String := To_String (Last_Element (Tag_Names));
                  begin
                     Delete_Last (Tag_Names);
                     End_Tag (Tag_Name,
                              Tag_Names,
                              Error_Message,
                              Is_Success);
                     if not Is_Success then
                        return;
                     end if;
                  end;
                  Shall_Ignore_Tag_Value := True;
                  Delete_Last (Shall_Ignore_Tag_Value_List);
               else
                  Initialize (Error_Message, "Expected '>', state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_Attribute_Name =>
               if CP = Standard.Character'Pos ('=') then
                  Attribute_Last_Index := Prev_Prev_P;
                  State_Id := Expecting_Attribute_Value_Quotation_Mark;
--                  Ada.Text_IO.Put_Line ("Extracted attribute name: '" & Contents (Attribute_First_Index..Attribute_Last_Index) & "'");
               elsif CP = Standard.Character'Pos (Ada.Characters.Latin_1.LF) then
                  Initialize (Error_Message, "New line is forbidden inside attribute name, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               elsif not Is_Special_Symbol (CP) then
                  null; -- Normal
               end if;
            when Expecting_Attribute_Value_Quotation_Mark =>
               if CP = Standard.Character'Pos ('"') or CP = Standard.Character'Pos (''') then
                  Attribute_Value_First_Index := P;
                  State_Id := Extracting_Attribute_Value;
               else
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_Attribute_Value =>
               if CP = Standard.Character'Pos ('"') or CP = Standard.Character'Pos (''') then
                  Attribute_Value_Last_Index := Prev_Prev_P;
                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
--                  Ada.Text_IO.Put_Line ("Extracted attribute value: '" & Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index) & "'");
                  Attribute (Contents (Attribute_First_Index..Attribute_Last_Index),
                             Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index),
                             Tag_Names,
                             Error_Message,
                             Is_Success);

                  if not Is_Success then
                     return;
                  end if;

               elsif CP = Standard.Character'Pos (Ada.Characters.Latin_1.LF) then
                  Initialize (Error_Message, "New line is forbidden inside attribute value, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value =>
               if CP = Standard.Character'Pos ('<') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L;
                  Tag_Value_Last_Index := Prev_Prev_P;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
               if CP = Standard.Character'Pos ('!') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation;
               elsif CP = Standard.Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               elsif Is_Special_Symbol (CP) then
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               else
                  -- Will ignore tag value, and start parsing child tag!
                  Shall_Ignore_Tag_Value := True;
                  State_Id := Extracting_Start_Tag_Name;
                  Start_Tag_Name_First_Index := Prev_P;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation =>
               if CP = Standard.Character'Pos ('[') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C =>
               if CP = Standard.Character'Pos ('C') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD =>
               if CP = Standard.Character'Pos ('D') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA =>
               if CP = Standard.Character'Pos ('A') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT =>
               if CP = Standard.Character'Pos ('T') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA =>
               if CP = Standard.Character'Pos ('A') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket =>
               if CP = Standard.Character'Pos ('[') then
                  State_Id := Extracting_CDATA;
                  Tag_Value_First_Index := P;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Extracting_CDATA =>
               if CP = Standard.Character'Pos (']') then
                  Tag_Value_Last_Index := Prev_Prev_P;
                  State_Id := Extracting_CDATA_Found_Square_Bracket;
               end if;
            when Extracting_CDATA_Found_Square_Bracket =>
               if CP = Standard.Character'Pos (']') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
               else
                  State_Id := Extracting_CDATA;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets =>
               if CP = Standard.Character'Pos ('>') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign;
               else
                  State_Id := Extracting_CDATA;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign =>
               if CP = Standard.Character'Pos ('<') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign;
               else
                  Initialize (Error_Message, "Expecting '<' followed by end tag but was something else, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign =>
               if CP = Standard.Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               else
                  Initialize (Error_Message, "Expecting '/' followed by end tag but was something else, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_End_Tag_Name =>
               if CP = Standard.Character'Pos ('>') then
                  End_Tag_Name_Last_Index := Prev_Prev_P;

                  declare
                     Tag_Name : constant Standard.String := To_String (Last_Element (Tag_Names));
                  begin
                     if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
                        Initialize (Error_Message, "Tag names does not match! Start tag is '" & Tag_Name &
                                                "', and end tag is '" & Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) & "' state " & State_Id'Img & " " & Contents (F..P));
                        Is_Success := False;
                        return;
                     end if;

                     Delete_Last (Tag_Names);

                     if not Shall_Ignore_Tag_Value then
                        End_Tag (Tag_Name,
                                 Contents (Tag_Value_First_Index..Tag_Value_Last_Index),
                                 Tag_Names,
                                 Error_Message,
                                 Is_Success);

                        if not Is_Success then
                           return;
                        end if;
                     else
                        End_Tag (Tag_Name,
                                 Tag_Names,
                                 Error_Message,
                                 Is_Success);

                        if not Is_Success then
                           return;
                        end if;
                     end if;

                     Shall_Ignore_Tag_Value := Last_Element (Shall_Ignore_Tag_Value_List);
                     Delete_Last (Shall_Ignore_Tag_Value_List);
                  end;

                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
               elsif CP = Standard.Character'Pos (Ada.Characters.Latin_1.LF) then
                  Initialize (Error_Message, "New line is forbidden inside attribute value, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               elsif Is_Special_Symbol (CP) then
                  Initialize (Error_Message, "Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
         end case;
      end loop;
   end;

   Is_Success := True;
   return;end Aida.XML.Parse_XML_File;
