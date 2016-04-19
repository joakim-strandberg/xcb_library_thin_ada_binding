with Strings_Edit.UTF8;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
--  with Ada.Text_IO;
with BC.Containers.Collections.Unmanaged;

procedure SXML.Generic_Parse_XML_File (Contents      : String;
                                       Error_Message : out Error_Message_Type;
                                       Is_Success    : out Boolean)
is
   use type Strings_Edit.UTF8.Code_Point;

   Tag_Names : DL.Collection;

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

   XML_File_Start_String : String := "<?xml version=""1.0"" encoding=""utf-8""?>";

   F : Natural := Contents'First;
   L : Natural := Contents'Last;

   package Abstract_Boolean_List is new BC.Containers (Item => Boolean);

   package Boolean_List_Collection is new Abstract_Boolean_List.Collections;

   package Boolean_List is new Boolean_List_Collection.Unmanaged;

   Shall_Ignore_Tag_Value_List : Boolean_List.Collection;

   function Is_Special_Symbol (CP : Strings_Edit.UTF8.Code_Point) return Boolean is
   begin
      if CP = Character'Pos ('<') then
         return True;
      elsif CP = Character'Pos ('>') then
         return True;
      elsif CP = Character'Pos ('/') then
         return True;
      elsif CP = Character'Pos ('"') then
         return True;
      else
         return False;
      end if;
   end Is_Special_Symbol;
begin
   if Contents (F..F+XML_File_Start_String'Length-1) /= XML_File_Start_String then
      Error_Message.Initialize ("File does not start with <?xml version...>");
      Is_Success := False;
      return;
   end if;

   State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

   declare
      P           : Integer := Strings_Edit.UTF8.Length (XML_File_Start_String) + 1;
      Prev_P      : Integer := P;
      Prev_Prev_P : Integer := Prev_P;

      CP      : Strings_Edit.UTF8.Code_Point := 0;

      Start_Tag_Name_First_Index : Integer;
      Start_Tag_Name_Last_Index  : Integer;

      Tag_Value_First_Index : Integer;
      Tag_Value_Last_Index  : Integer;

      End_Tag_Name_First_Index : Integer;
      End_Tag_Name_Last_Index  : Integer;

      Attribute_First_Index : Integer;
      Attribute_Last_Index  : Integer;

      Attribute_Value_First_Index : Integer;
      Attribute_Value_Last_Index  : Integer;

      Shall_Ignore_Tag_Value : Boolean;
   begin
      while P < Contents'Last loop
         Prev_Prev_P := Prev_P;

         Prev_P := P;

         Strings_Edit.UTF8.Get (Source  => Contents,
                                Pointer => P,
                                Value   => CP);

--                     Ada.Text_IO.Put_Line ("Extracted:" & Strings_Edit.UTF8.Image (CP) & ", state " & State_Id'Img);
--                     Ada.Text_IO.Put (Strings_Edit.UTF8.Image (CP));

         case State_Id is
            when Searching_For_XML_Start_String =>
               Error_Message.Initialize ("It should not be possible to be in the state " & State_Id'Img);
               Is_Success := False;
               return;
            when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
               if CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (' ') then
                  null; -- Normal
               elsif CP = Character'Pos ('<') then
                  State_Id := Found_Less_Sign;
               else
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Found_Less_Sign =>
               if CP = Character'Pos ('!') then
                  State_Id := Found_Less_Followed_By_Exclamation_Sign;
               elsif CP = Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               elsif not Is_Special_Symbol (CP) then
                  State_Id := Extracting_Start_Tag_Name;
                  Start_Tag_Name_First_Index := Prev_P;
               else
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Found_Less_Followed_By_Exclamation_Sign =>
               if CP = Character'Pos ('-') then
                  State_Id := Found_Less_Followed_By_Exclamation_And_Dash_Sign;
               else
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
               if CP = Character'Pos ('-') then
                  State_Id := Ignore_Comment;
               else
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Ignore_Comment =>
               if CP = Character'Pos ('-') then
                  State_Id := Ignore_Comment_Followed_By_Dash_Sign;
               end if;
            when Ignore_Comment_Followed_By_Dash_Sign =>
               if CP = Character'Pos ('-') then
                  State_Id := Ignore_Comment_Followed_By_Two_Dash_Signs;
               else
                  State_Id := Ignore_Comment;
               end if;
            when Ignore_Comment_Followed_By_Two_Dash_Signs =>
               if CP = Character'Pos ('>') then
                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
               else
                  State_Id := Ignore_Comment;
               end if;
            when Extracting_Start_Tag_Name =>
               if CP = Character'Pos (' ') then
                  Start_Tag_Name_Last_Index := Prev_Prev_P;
                  Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                             Tag_Names,
                             Error_Message,
                             Is_Success);

                  if not Is_Success then
                     return;
                  end if;

                  DL.Append (C    => Tag_Names,
                             Elem => Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index));


                  Boolean_List.Append (C    => Shall_Ignore_Tag_Value_List,
                                       Elem => Shall_Ignore_Tag_Value);

                  Shall_Ignore_Tag_Value := False;

                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
               elsif CP = Character'Pos ('>') then
                  Start_Tag_Name_Last_Index := Prev_Prev_P;
                  Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                             Tag_Names,
                             Error_Message,
                             Is_Success);

                  if not Is_Success then
                     return;
                  end if;

                  DL.Append (C    => Tag_Names,
                             Elem => Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index));

                  Boolean_List.Append (C    => Shall_Ignore_Tag_Value_List,
                                       Elem => Shall_Ignore_Tag_Value);

                  Shall_Ignore_Tag_Value := False;
                  Tag_Value_First_Index := P;

                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               elsif Is_Special_Symbol (CP) then
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Expecting_G_Sign_Or_Extracting_Attributes =>
               if CP = Character'Pos (' ') or CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  null; -- Normal
               elsif CP = Character'Pos ('>') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  Tag_Value_First_Index := P;
               elsif CP = Character'Pos ('/') then
                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
               elsif not Is_Special_Symbol (CP) then
                  Attribute_First_Index := Prev_P;
                  State_Id := Extracting_Attribute_Name;
               end if;
            when Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash =>
               if CP = Character'Pos ('>') then
                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;


                  declare
                     Tag_Name : String := DL.Last (Tag_Names);
                  begin
                     DL.Remove (C        => Tag_Names,
                                At_Index => DL.Length (Tag_Names));
                     End_Tag (Tag_Name,
                              Tag_Names,
                              Error_Message,
                              Is_Success);
                     if not Is_Success then
                        return;
                     end if;
                  end;
                  Shall_Ignore_Tag_Value := True;
               else
                  Error_Message.Initialize ("Expected '>', state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_Attribute_Name =>
               if CP = Character'Pos ('=') then
                  Attribute_Last_Index := Prev_Prev_P;
                  State_Id := Expecting_Attribute_Value_Quotation_Mark;
--                  Ada.Text_IO.Put_Line ("Extracted attribute name: '" & Contents (Attribute_First_Index..Attribute_Last_Index) & "'");
               elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  Error_Message.Initialize ("New line is forbidden inside attribute name, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               elsif not Is_Special_Symbol (CP) then
                  null; -- Normal
               end if;
            when Expecting_Attribute_Value_Quotation_Mark =>
               if CP = Character'Pos ('"') or CP = Character'Pos (''') then
                  Attribute_Value_First_Index := P;
                  State_Id := Extracting_Attribute_Value;
               else
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_Attribute_Value =>
               if CP = Character'Pos ('"') or CP = Character'Pos (''') then
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

               elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  Error_Message.Initialize ("New line is forbidden inside attribute value, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value =>
               if CP = Character'Pos ('<') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L;
                  Tag_Value_Last_Index := Prev_Prev_P;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
               if CP = Character'Pos ('!') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation;
               elsif CP = Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               elsif Is_Special_Symbol (CP) then
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               else
                  -- Will ignore tag value, and start parsing child tag!
                  Shall_Ignore_Tag_Value := True;
                  State_Id := Extracting_Start_Tag_Name;
                  Start_Tag_Name_First_Index := Prev_P;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation =>
               if CP = Character'Pos ('[') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C =>
               if CP = Character'Pos ('C') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD =>
               if CP = Character'Pos ('D') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA =>
               if CP = Character'Pos ('A') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT =>
               if CP = Character'Pos ('T') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA =>
               if CP = Character'Pos ('A') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket =>
               if CP = Character'Pos ('[') then
                  State_Id := Extracting_CDATA;
                  Tag_Value_First_Index := P;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Extracting_CDATA =>
               if CP = Character'Pos (']') then
                  Tag_Value_Last_Index := Prev_Prev_P;
                  State_Id := Extracting_CDATA_Found_Square_Bracket;
               end if;
            when Extracting_CDATA_Found_Square_Bracket =>
               if CP = Character'Pos (']') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
               else
                  State_Id := Extracting_CDATA;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets =>
               if CP = Character'Pos ('>') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign;
               else
                  State_Id := Extracting_CDATA;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign =>
               if CP = Character'Pos ('<') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign;
               else
                  Error_Message.Initialize ("Expecting '<' followed by end tag but was something else, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign =>
               if CP = Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               else
                  Error_Message.Initialize ("Expecting '/' followed by end tag but was something else, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
            when Extracting_End_Tag_Name =>
               if CP = Character'Pos ('>') then
                  End_Tag_Name_Last_Index := Prev_Prev_P;

                  declare
                     Tag_Name : String := DL.Last (Tag_Names);
                  begin
                     if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
                        Error_Message.Initialize ("Tag names does not match! Start tag is '" & Tag_Name &
                                                "', and end tag is '" & Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) & "' state " & State_Id'Img & " " & Contents (F..P));
                        Is_Success := False;
                        return;
                     end if;

                     DL.Remove (C        => Tag_Names,
                                At_Index => DL.Length (Tag_Names));

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

                     Shall_Ignore_Tag_Value := Boolean_List.Last (Shall_Ignore_Tag_Value_List);
                     Boolean_List.Remove (C        => Shall_Ignore_Tag_Value_List,
                                          At_Index => Boolean_List.Length (Shall_Ignore_Tag_Value_List));
                  end;

                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
               elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  Error_Message.Initialize ("New line is forbidden inside attribute value, state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               elsif Is_Special_Symbol (CP) then
                  Error_Message.Initialize ("Unexpected UTF8 symbol (code point" & CP'Img & "), state " & State_Id'Img & " " & Contents (F..P));
                  Is_Success := False;
                  return;
               end if;
         end case;
      end loop;
   end;

   Is_Success := True;
   return;
end SXML.Generic_Parse_XML_File;
