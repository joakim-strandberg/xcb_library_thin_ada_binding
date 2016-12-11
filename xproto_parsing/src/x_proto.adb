with Ada.Strings.Unbounded;

package body X_Proto is

   package body Struct is

      function Name (This : T) return Fs.Const_Name_Ptr is
      begin
         return This.Name'Unchecked_Access;
      end Name;

      function Members (This : T) return Fs.Member_Vectors.Vector is
      begin
         return (This.Members);
      end Members;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.Name := (Exists => True,
                       Value  => Name);
      end Set_Name;

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr) is
      begin
         This.Members.Append (Member);
      end Append_Member;

   end Struct;

   package body X_Id is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

   end X_Id;

   package body Type_P is

      function Value (This : T) return Fs.Value_Const_Ptr is
      begin
         return This.My_Value'Unchecked_Access;
      end Value;

      procedure Set_Value (This : in out T;
                           Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Value := (Exists => True,
                           Value  => Name);
      end Set_Value;

   end Type_P;

   package body X_Id_Union is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Kinds (This : T) return Fs.Type_Vector_Const_Ptr is
      begin
         return This.My_Kinds'Unchecked_Access;
      end Kinds;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Append_Kind (This : in out T;
                             Kind : Type_P.Ptr) is
      begin
         This.My_Kinds.Append (Kind);
      end Append_Kind;

   end X_Id_Union;

   package body Field is

      function Kind (This : T) return Fs.Kind_Const_Ptr is
      begin
         return This.My_Kind'Unchecked_Access;
      end Kind;

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Enum (This : T) return Fs.Enum_Const_Ptr is
      begin
         return This.My_Enum'Unchecked_Access;
      end Enum;

      function Mask (This : T) return Fs.Mask_Const_Ptr is
      begin
         return This.My_Mask'Unchecked_Access;
      end Mask;

      function Alt_Enum (This : T) return Fs.Alt_Enum_Const_Ptr is
      begin
         return This.My_Alt_Enum'Unchecked_Access;
      end Alt_Enum;

      function Value (This : T) return Fs.Value_Const_Ptr is
      begin
         return This.My_Value'Unchecked_Access;
      end Value;

      procedure Set_Kind (This : in out T;
                          Kind : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Kind := (Exists => True,
                          Value  => Kind);
      end Set_Kind;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Enum (This : in out T;
                          Enum : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Enum := (Exists => True,
                          Value  => Enum);
      end Set_Enum;

      procedure Set_Mask (This : in out T;
                          Mask : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Mask := (Exists => True,
                          Value  => Mask);
      end Set_Mask;

      procedure Set_Value (This  : in out T;
                           Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Value := (Exists => True,
                           Value  => Value);
      end Set_Value;

      procedure Set_Alt_Enum (This     : in out T;
                              Alt_Enum : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Alt_Enum := (Exists => True,
                              Value  => Alt_Enum);
      end Set_Alt_Enum;

   end Field;

   package body Pad is

      function Bytes (This : T) return Fs.Bytes_Const_Ptr is
      begin
         return This.My_Bytes'Unchecked_Access;
      end Bytes;

      procedure Set_Bytes (This  : in out T;
                           Bytes : Positive) is
      begin
         This.My_Bytes := (Exists => True,
                           Value  => Bytes);
      end Set_Bytes;

   end Pad;

   package body Enum is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Items (This : T) return Fs.Items_Const_Ptr is
      begin
         return This.My_Items'Unchecked_Access;
      end Items;

      function Documentations (This : T) return Fs.Documentations_Const_Ptr is
      begin
         return This.My_Documentations'Unchecked_Access;
      end Documentations;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Append_Item (This   : in out T;
                             Item_V : Item.Ptr) is
      begin
         Fs.Item_Vectors.Append (Container => This.My_Items,
                                 New_Item  => Item_V);
      end Append_Item;

      procedure Append_Documentation (This            : in out T;
                                      Documentation_V : Documentation.Ptr) is
      begin
         Fs.Documentation_Vectors.Append (Container => This.My_Documentations,
                                          New_Item  => Documentation_V);
      end Append_Documentation;

   end Enum;

   package body Item is

      function Kind_Id (This : T) return Fs.Kind_Id_Type is
      begin
         return This.My_Kind_Id;
      end Kind_Id;

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Value (This : T) return Value_Type is
      begin
         return This.My_Value;
      end Value;

      function Bit (This : T) return Fs.Bit_Type is
      begin
         return This.My_Bit;
      end Bit;

      procedure Set_Kind_Id (This    : in out T;
                             Kind_Id : Fs.Kind_Id_Type) is
      begin
         This.My_Kind_Id := Kind_Id;
      end Set_Kind_Id;


      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Value (This  : in out T;
                           Value : Value_Type) is
      begin
         This.My_Value := Value;
      end Set_Value;

      procedure Set_Bit (This : in out T;
                         Bit  : Fs.Bit_Type) is
      begin
         This.My_Bit := Bit;
      end Set_Bit;

   end Item;

   package body List is

      function Kind (This : T) return Fs.Kind_Const_Ptr is
      begin
         return This.My_Kind'Unchecked_Access;
      end Kind;

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Members (This : T) return Fs.Members_Const_Ptr is
      begin
         return This.My_Members'Unchecked_Access;
      end Members;

      procedure Set_Kind (This : in out T;
                          Kind : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Kind := (Exists => True,
                          Value  => Kind);
      end Set_Kind;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr) is
      begin
         Fs.Member_Vectors.Append (Container => This.My_Members,
                                   New_Item  => Member);
      end Append_Member;

   end List;

   package body Operation is

      function Op (This : T) return Fs.Op_Const_Ptr is
      begin
         return This.My_Op'Unchecked_Access;
      end Op;

      function Members (This : T) return Fs.Members_Const_Ptr is
      begin
         return This.My_Members'Unchecked_Access;
      end Members;

      procedure Set_Op (This : in out T;
                        Op   : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Op := (Exists => True,
                        Value  => Op);
      end Set_Op;

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr) is
      begin
         Fs.Member_Vectors.Append (Container => This.My_Members,
                                   New_Item  => Member);
      end Append_Member;

   end Operation;

   package body Type_Definition is

      function Old_Name (This : T) return Fs.Old_Name_Const_Ptr is
      begin
         return This.My_Old_Name'Unchecked_Access;
      end Old_Name;

      function New_Name (This : T) return Fs.New_Name_Const_Ptr is
      begin
         return This.My_New_Name'Unchecked_Access;
      end New_Name;

      procedure Set_Old_Name (This : in out T;
                              Old_Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Old_Name := (Exists => True,
                              Value  => Old_Name);
      end Set_Old_Name;

      procedure Set_New_Name (This     : in out T;
                              New_Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_New_Name := (Exists => True,
                              Value  => New_Name);
      end Set_New_Name;

   end Type_Definition;

   package body Xcb is

      function Header (This : T) return Fs.Header_Const_Ptr is
      begin
         return This.My_Header'Unchecked_Access;
      end Header;

      function Structs (This : T) return Fs.Structs_Const_Ptr is
      begin
         return This.My_Structs'Unchecked_Access;
      end Structs;

      function X_Ids (This : T) return Fs.X_Ids_Const_Ptr is
      begin
         return This.My_X_Ids'Unchecked_Access;
      end X_Ids;

      function X_Id_Unions (This : T) return Fs.X_Id_Unions_Const_Ptr is
      begin
         return This.My_X_Id_Unions'Unchecked_Access;
      end X_Id_Unions;

      function Type_Definitions (This : T) return Fs.Type_Definitions_Const_Ptr is
      begin
         return This.My_Type_Definitions'Unchecked_Access;
      end Type_Definitions;

      function Enums (This : T) return Fs.Enums_Const_Ptr is
      begin
         return This.My_Enums'Unchecked_Access;
      end Enums;

      function Events (This : T) return Fs.Events_Const_Ptr is
      begin
         return This.My_Events'Unchecked_Access;
      end Events;

      function Event_Copies (This : T) return Fs.Event_Copies_Const_Ptr is
      begin
         return This.My_Event_Copies'Unchecked_Access;
      end Event_Copies;

      function Unions (This : T) return Fs.Unions_Const_Ptr is
      begin
         return This.My_Unions'Unchecked_Access;
      end Unions;

      function Errors (This : T) return Fs.Errors_Const_Ptr is
      begin
         return This.My_Errors'Unchecked_Access;
      end Errors;

      function Error_Copies (This : T) return Fs.Error_Copies_Const_Ptr is
      begin
         return This.My_Error_Copies'Unchecked_Access;
      end Error_Copies;

      function Requests (This : T) return Fs.Requests_Const_Ptr is
      begin
         return This.My_Requests'Unchecked_Access;
      end Requests;

      procedure Set_Header (This : in out T;
                            Text : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Header := (Exists => True,
                            Value  => Text);
      end Set_Header;

      procedure Append_Struct (This : in out T;
                               Item : Struct.Ptr) is
      begin
         Fs.Struct_Vector.Append (This      => This.My_Structs,
                                  New_Item  => Item);
      end Append_Struct;

      procedure Append_X_Id (This : in out T;
                             Item : X_Id.Ptr) is
      begin
         Fs.X_Id_Vectors.Append (Container => This.My_X_Ids,
                                 New_Item  => Item);
      end Append_X_Id;

      procedure Append_X_Id_Union (This : in out T;
                                   Item : X_Id_Union.Ptr) is
      begin
         Fs.X_Id_Union_Vectors.Append (Container => This.My_X_Id_Unions,
                                       New_Item  => Item);
      end Append_X_Id_Union;

      procedure Append_Type_Definition (This : in out T;
                                        Item : Type_Definition.Ptr) is
      begin
         Fs.Type_Definition_Vectors.Append (Container => This.My_Type_Definitions,
                                            New_Item  => Item);
      end Append_Type_Definition;

      procedure Append_Enum (This : in out T;
                             Item : Enum.Ptr) is
      begin
         Fs.Enum_Vectors.Append (Container => This.My_Enums,
                                 New_Item  => Item);
      end Append_Enum;

      procedure Append_Event (This : in out T;
                              Item : Event.Ptr) is
      begin
         Fs.Event_Vectors.Append (Container => This.My_Events,
                                  New_Item  => Item);
      end Append_Event;

      procedure Append_Event_Copy (This : in out T;
                                   Item : Event_Copy.Ptr) is
      begin
         Fs.Event_Copy_Vectors.Append (Container => This.My_Event_Copies,
                                       New_Item  => Item);
      end Append_Event_Copy;

      procedure Append_Union (This : in out T;
                              Item : Union.Ptr) is
      begin
         Fs.Union_Vectors.Append (Container => This.My_Unions,
                                  New_Item  => Item);
      end Append_Union;

      procedure Append_Error (This : in out T;
                              Item : Error.Ptr) is
      begin
         Fs.Error_Vectors.Append (Container => This.My_Errors,
                                  New_Item  => Item);
      end Append_Error;

      procedure Append_Error_Copy (This : in out T;
                                   Item : Error_Copy.Ptr) is
      begin
         Fs.Error_Copy_Vectors.Append (Container => This.My_Error_Copies,
                                       New_Item  => Item);
      end Append_Error_Copy;

      procedure Append_Request (This : in out T;
                                Item : Request.Ptr) is
      begin
         Fs.Request_Vectors.Append (Container => This.My_Requests,
                                    New_Item  => Item);
      end Append_Request;

   end Xcb;

   package body Event is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Number (This : T) return Fs.Number_Const_Ptr is
      begin
         return This.My_Number'Unchecked_Access;
      end Number;

      function No_Sequence_Number (This : T) return Fs.No_Sequence_Number_Const_Ptr is
      begin
         return This.My_No_Sequence_Number'Unchecked_Access;
      end No_Sequence_Number;

      function XGE (This : T) return Fs.XGE_Const_Ptr is
      begin
         return This.My_XGE'Unchecked_Access;
      end XGE;

      function Members (This : T) return Fs.Members_Const_Ptr is
      begin
         return This.My_Members'Unchecked_Access;
      end Members;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Value : Natural) is
      begin
         This.My_Number := (Exists => True,
                            Value  => Value);
      end Set_Number;

      procedure Set_No_Sequence_Number (This  : in out T;
                                        Value : Boolean) is
      begin
         This.My_No_Sequence_Number := (Exists => True,
                                        Value  => Value);
      end Set_No_Sequence_Number;

      procedure Set_XGE (This  : in out T;
                         Value : Boolean) is
      begin
         This.My_XGE := (Exists => True,
                         Value  => Value);
      end Set_XGE;

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr) is
      begin
         Fs.Member_Vectors.Append (Container => This.My_Members,
                                   New_Item  => Member);
      end Append_Member;

   end Event;

   package body Event_Copy is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Number (This : T) return Fs.Number_Const_Ptr is
      begin
         return This.My_Number'Unchecked_Access;
      end Number;

      function Ref (This : T) return Fs.Ref_Const_Ptr is
      begin
         return This.My_Ref'Unchecked_Access;
      end Ref;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Number (This  : in out T;
                            Value : Natural) is
      begin
         This.My_Number := (Exists => True,
                            Value  => Value);
      end Set_Number;

      procedure Set_Ref (This : in out T;
                         Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Ref := (Exists => True,
                         Value  => Name);
      end Set_Ref;

   end Event_Copy;

   package body Union is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Children (This : T) return Fs.Children_Const_Ptr is
      begin
         return This.My_Children'Unchecked_Access;
      end Children;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr) is
      begin
         Fs.Child_Vectors.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Union;

   package body Error is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Number (This : T) return Fs.Number_Const_Ptr is
      begin
         return This.My_Number'Unchecked_Access;
      end Number;

      function Kind (This : T) return Fs.Kind_Const_Ptr is
      begin
         return This.My_Kind'Unchecked_Access;
      end Kind;

      function Value (This : T) return Fs.Value_Const_Ptr is
      begin
         return This.My_Value'Unchecked_Access;
      end Value;

      function Children (This : T) return Fs.Children_Const_Ptr is
      begin
         return This.My_Children'Unchecked_Access;
      end Children;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Value : Natural) is
      begin
         This.My_Number := (Exists => True,
                            Value  => Value);
      end Set_Number;

      procedure Set_Kind (This  : in out T;
                          Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Kind := (Exists => True,
                          Value  => Value);
      end Set_Kind;

      procedure Set_Value (This  : in out T;
                           Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Value := (Exists => True,
                          Value  => Value);
      end Set_Value;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr) is
      begin
         Fs.Child_Vectors.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Error;

   package body Error_Copy is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Number (This : T) return Fs.Number_Const_Ptr is
      begin
         return This.My_Number'Unchecked_Access;
      end Number;

      function Ref (This : T) return Fs.Ref_Const_Ptr is
      begin
         return This.My_Ref'Unchecked_Access;
      end Ref;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Value : Natural) is
      begin
         This.My_Number := (Exists => True,
                            Value  => Value);
      end Set_Number;

      procedure Set_Ref (This  : in out T;
                         Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Ref := (Exists => True,
                         Value  => Value);
      end Set_Ref;

   end Error_Copy;

   package body Request is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Op_Code (This : T) return Fs.Op_Code_Const_Ptr is
      begin
         return This.My_Op_Code'Unchecked_Access;
      end Op_Code;

      function Shall_Combine_Adjacent (This : T) return Fs.Shall_Combine_Adjacent_Const_Ptr is
      begin
         return This.My_Shall_Combine_Adjacent'Unchecked_Access;
      end Shall_Combine_Adjacent;

      function Children (This : T) return Fs.Children_Const_Ptr is
      begin
         return This.My_Children'Unchecked_Access;
      end Children;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Op_Code (This  : in out T;
                             Value : Natural) is
      begin
         This.My_Op_Code := (Exists => True,
                             Value  => Value);
      end Set_Op_Code;

      procedure Set_Shall_Combine_Adjacent (This  : in out T;
                                            Value : Boolean) is
      begin
         This.My_Shall_Combine_Adjacent := (Exists => True,
                                            Value  => Value);
      end Set_Shall_Combine_Adjacent;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr) is
      begin
         Fs.Child_Vectors.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Request;

   package body Expression_Field is

      function Kind (This : T) return Fs.Kind_Const_Ptr is
      begin
         return This.My_Kind'Unchecked_Access;
      end Kind;

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Children (This : T) return Fs.Children_Const_Ptr is
      begin
         return This.My_Children'Unchecked_Access;
      end Children;

      procedure Set_Kind (This  : in out T;
                          Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Kind := (Exists => True,
                          Value  => Value);
      end Set_Kind;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr) is
      begin
         Fs.Child_Vectors.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Expression_Field;

   package body Documentation is

      function Brief_Description (This : T) return Fs.Brief_Description_Const_Ptr is
      begin
         return This.My_Brief_Description'Unchecked_Access;
      end Brief_Description;

      function Description (This : T) return Fs.Description_Const_Ptr is
      begin
         return This.My_Description'Unchecked_Access;
      end Description;

      function Members (This : T) return Fs.Members_Const_Ptr is
      begin
         return This.My_Members'Unchecked_Access;
      end Members;

      procedure Set_Brief_Description (This  : in out T;
                                       Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Brief_Description := (Exists => True,
                                       Value  => Value);
      end Set_Brief_Description;

      procedure Set_Description (This  : in out T;
                                 Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Description := (Exists => True,
                                 Value  => Value);
      end Set_Description;

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr) is
      begin
         Fs.Member_Vectors.Append (Container => This.My_Members,
                                   New_Item  => Member);
      end Append_Member;

   end Documentation;

   package body Example is

      function Value (This : T) return Fs.Value_Const_Ptr is
      begin
         return This.My_Value'Unchecked_Access;
      end Value;

      procedure Set_Value (This  : in out T;
                           Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Value := (Exists => True,
                           Value  => Value);
      end Set_Value;

   end Example;

   package body See is

      function Kind (This : T) return Fs.Kind_Const_Ptr is
      begin
         return This.My_Kind'Unchecked_Access;
      end Kind;

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      procedure Set_Kind (This  : in out T;
                          Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Kind := (Exists => True,
                          Value  => Value);
      end Set_Kind;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

   end See;

   package body Value_Param is

      function Mask_Kind (This : T) return Fs.Mask_Kind_Const_Ptr is
      begin
         return This.My_Mask_Kind'Unchecked_Access;
      end Mask_Kind;

      function Mask_Name (This : T) return Fs.Mask_Name_Const_Ptr is
      begin
         return This.My_Mask_Name'Unchecked_Access;
      end Mask_Name;

      function List_Name (This : T) return Fs.List_Name_Const_Ptr is
      begin
         return This.My_List_Name'Unchecked_Access;
      end List_Name;

      procedure Set_Mask_Kind (This  : in out T;
                               Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Mask_Kind := (Exists => True,
                               Value  => Value);
      end Set_Mask_Kind;

      procedure Set_Mask_Name (This  : in out T;
                               Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Mask_Name := (Exists => True,
                               Value  => Value);
      end Set_Mask_Name;

      procedure Set_List_Name (This : in out T;
                               Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_List_Name := (Exists => True,
                               Value  => Value);
      end Set_List_Name;

   end Value_Param;

   package body Reply is

      function Children (This : T) return Fs.Children_Const_Ptr is
      begin
         return This.My_Children'Unchecked_Access;
      end Children;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr) is
      begin
         Fs.Child_Vectors.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Reply;

end X_Proto;
