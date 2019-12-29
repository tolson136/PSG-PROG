 
 /*------------------------------------------------------------------------
    File        : Billing
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Tim
    Created     : Tue Nov 26 20:10:29 CST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

/*
DEFINE VAR qh AS HANDLE NO-UNDO. 
CONSTRUCTOR PUBLIC Form1 ( ): 
    SUPER(). 
    InitializeComponent ( ).
    CREATE QUERY qh. 
    qh:SET-BUFFERS(BUFFER Customer:HANDLE).
    qh:QUERY-PREPARE("FOR EACH customer where customer.state = 'TX'").
    qh:QUERY-OPEN ().
    proBindingSource1:handle= qh.
END CONSTRUCTOR.

DESTRUCTOR Form1 ( ):
    IF VALID-HANDLE(qh) THEN DO: 
       qh:QUERY-CLOSE().
       DELETE OBJECT qh.
    END.
END DESTRUCTOR.
*/


USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Billing INHERITS Form: 
    
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE bsTicket AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE dataGridView1 AS System.Windows.Forms.DataGridView NO-UNDO.

        
    CONSTRUCTOR PUBLIC Billing (  ):
        
        
        SUPER().
        InitializeComponent().
        CREATE QUERY qTicket. 
        qTicket:SET-BUFFERS(BUFFER Ticket:HANDLE).


    METHOD PRIVATE VOID InitializeComponent ( ):
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:dataGridView1 = NEW System.Windows.Forms.DataGridView().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* dataGridView1 */
        /*  */
        THIS-OBJECT:dataGridView1:ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode:AutoSize.
        THIS-OBJECT:dataGridView1:Location = NEW System.Drawing.Point(39, 51).
        THIS-OBJECT:dataGridView1:Name = "dataGridView1".
        THIS-OBJECT:dataGridView1:Size = NEW System.Drawing.Size(240, 150).
        THIS-OBJECT:dataGridView1:TabIndex = 0.
        /*  */
        /* Billing */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:dataGridView1).
        THIS-OBJECT:Name = "Billing".
        THIS-OBJECT:Text = "Billing".
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.


    METHOD PRIVATE VOID InitializeComponent ( ):
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("TICKET").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("ACCT-RCV").
        THIS-OBJECT:dataGridView1 = NEW System.Windows.Forms.DataGridView().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* dataGridView1 */
        /*  */
        THIS-OBJECT:dataGridView1:ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode:AutoSize.
        THIS-OBJECT:dataGridView1:Location = NEW System.Drawing.Point(39, 51).
        THIS-OBJECT:dataGridView1:Name = "dataGridView1".
        THIS-OBJECT:dataGridView1:Size = NEW System.Drawing.Size(240, 150).
        THIS-OBJECT:dataGridView1:TabIndex = 0.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 46 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("ADDR-1", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("ADDR-2", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("ADDR-3", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("ADDR-4", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("ADDR-5", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("BAL-1", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("BAL-2", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[8] = NEW Progress.Data.ColumnPropDesc("BAL-3", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[9] = NEW Progress.Data.ColumnPropDesc("BAL-4", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[10] = NEW Progress.Data.ColumnPropDesc("C-NAME", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[11] = NEW Progress.Data.ColumnPropDesc("C-STATUS", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[12] = NEW Progress.Data.ColumnPropDesc("C-TYPE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[13] = NEW Progress.Data.ColumnPropDesc("CITY-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[14] = NEW Progress.Data.ColumnPropDesc("CNTY-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[15] = NEW Progress.Data.ColumnPropDesc("COMMENTS", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[16] = NEW Progress.Data.ColumnPropDesc("COMP#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[17] = NEW Progress.Data.ColumnPropDesc("COST-YTD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[18] = NEW Progress.Data.ColumnPropDesc("CST-LAST", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[19] = NEW Progress.Data.ColumnPropDesc("CUST#", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[20] = NEW Progress.Data.ColumnPropDesc("DIV#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[21] = NEW Progress.Data.ColumnPropDesc("EXP-DATE", "null", Progress.Data.DataType:DATE).
        arrayvar1[22] = NEW Progress.Data.ColumnPropDesc("EXPIRE-DT", "null", Progress.Data.DataType:DATE).
        arrayvar1[23] = NEW Progress.Data.ColumnPropDesc("Fuel%", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[24] = NEW Progress.Data.ColumnPropDesc("Fuel_Sur", "null", Progress.Data.DataType:LOGICAL).
        arrayvar1[25] = NEW Progress.Data.ColumnPropDesc("FUTR-DUE", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[26] = NEW Progress.Data.ColumnPropDesc("LP-DATE", "null", Progress.Data.DataType:DATE).
        arrayvar1[27] = NEW Progress.Data.ColumnPropDesc("LST-DATE", "null", Progress.Data.DataType:DATE).
        arrayvar1[28] = NEW Progress.Data.ColumnPropDesc("OPEN-DT", "null", Progress.Data.DataType:DATE).
        arrayvar1[29] = NEW Progress.Data.ColumnPropDesc("PO#", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[30] = NEW Progress.Data.ColumnPropDesc("ROUTE#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[31] = NEW Progress.Data.ColumnPropDesc("SHRT-NAM", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[32] = NEW Progress.Data.ColumnPropDesc("SLS#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[33] = NEW Progress.Data.ColumnPropDesc("SLS-LAST", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[34] = NEW Progress.Data.ColumnPropDesc("SLS-YTD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[35] = NEW Progress.Data.ColumnPropDesc("SRVC-CHG", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[36] = NEW Progress.Data.ColumnPropDesc("SRVC-DAYS", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[37] = NEW Progress.Data.ColumnPropDesc("SRVC-DUE", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[38] = NEW Progress.Data.ColumnPropDesc("SRVC-PER", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[39] = NEW Progress.Data.ColumnPropDesc("SRVC-YTD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[40] = NEW Progress.Data.ColumnPropDesc("STATE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[41] = NEW Progress.Data.ColumnPropDesc("STMT-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[42] = NEW Progress.Data.ColumnPropDesc("SUB#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[43] = NEW Progress.Data.ColumnPropDesc("TELE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[44] = NEW Progress.Data.ColumnPropDesc("TERM-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[45] = NEW Progress.Data.ColumnPropDesc("TOT-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[46] = NEW Progress.Data.ColumnPropDesc("ZIP", "null", Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar1.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
        arrayvar2[1] = tableDesc2.
        tableDesc1:ChildTables = arrayvar2.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 42 NO-UNDO.
        arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("BudgetedHours", "Budgeted Hrs", Progress.Data.DataType:DECIMAL).
        arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("CABLE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("CABLE-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[4] = NEW Progress.Data.ColumnPropDesc("COMP#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[5] = NEW Progress.Data.ColumnPropDesc("CUST#", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[6] = NEW Progress.Data.ColumnPropDesc("DATE-PRT", "null", Progress.Data.DataType:DATE).
        arrayvar3[7] = NEW Progress.Data.ColumnPropDesc("DATE-RET", "null", Progress.Data.DataType:DATE).
        arrayvar3[8] = NEW Progress.Data.ColumnPropDesc("DIV#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[9] = NEW Progress.Data.ColumnPropDesc("DL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[10] = NEW Progress.Data.ColumnPropDesc("DL-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[11] = NEW Progress.Data.ColumnPropDesc("DL-BUD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[12] = NEW Progress.Data.ColumnPropDesc("DL-LAST", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[13] = NEW Progress.Data.ColumnPropDesc("FREQ", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[14] = NEW Progress.Data.ColumnPropDesc("ITEM#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[15] = NEW Progress.Data.ColumnPropDesc("M-NAME", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[16] = NEW Progress.Data.ColumnPropDesc("M-NUM", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[17] = NEW Progress.Data.ColumnPropDesc("MONTH#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[18] = NEW Progress.Data.ColumnPropDesc("MTD-VOL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[19] = NEW Progress.Data.ColumnPropDesc("OTH-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[20] = NEW Progress.Data.ColumnPropDesc("OTHER", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[21] = NEW Progress.Data.ColumnPropDesc("PRISEC", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[22] = NEW Progress.Data.ColumnPropDesc("PROPSL#", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[23] = NEW Progress.Data.ColumnPropDesc("PRT", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[24] = NEW Progress.Data.ColumnPropDesc("RET", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[25] = NEW Progress.Data.ColumnPropDesc("ROUTE#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[26] = NEW Progress.Data.ColumnPropDesc("SCAFF-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[27] = NEW Progress.Data.ColumnPropDesc("SCAFFOLD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[28] = NEW Progress.Data.ColumnPropDesc("SUB#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[29] = NEW Progress.Data.ColumnPropDesc("T-INDX", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[30] = NEW Progress.Data.ColumnPropDesc("T-STAT", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[31] = NEW Progress.Data.ColumnPropDesc("TicketDate", "Ticket Date", Progress.Data.DataType:DATE).
        arrayvar3[32] = NEW Progress.Data.ColumnPropDesc("TicketDateConfirmed", "Ticket Date Confirmed", Progress.Data.DataType:LOGICAL).
        arrayvar3[33] = NEW Progress.Data.ColumnPropDesc("TOT-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[34] = NEW Progress.Data.ColumnPropDesc("TOT-AMT-REM", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[35] = NEW Progress.Data.ColumnPropDesc("wk#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[36] = NEW Progress.Data.ColumnPropDesc("WK-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[37] = NEW Progress.Data.ColumnPropDesc("WK-DL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[38] = NEW Progress.Data.ColumnPropDesc("WK-DL-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[39] = NEW Progress.Data.ColumnPropDesc("WK-DL-BUD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[40] = NEW Progress.Data.ColumnPropDesc("WK-START", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[41] = NEW Progress.Data.ColumnPropDesc("WK-USED", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[42] = NEW Progress.Data.ColumnPropDesc("WTD-VOL", "null", Progress.Data.DataType:DECIMAL).
        tableDesc1:Columns = arrayvar3.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* Billing */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(448, 406).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:dataGridView1).
        THIS-OBJECT:Name = "Billing".
        THIS-OBJECT:Text = "Billing".
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.
        qTicket:QUERY-PREPARE("FOR EACH ticket).
        qTicket:QUERY-OPEN ().
        bsTicket:handle= qTicket.
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent(  ):
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("Table").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("ACCT-RCV").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc3 = NEW Progress.Data.TableDesc("TICKET").
        THIS-OBJECT:bsTicket = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        CAST(THIS-OBJECT:bsTicket, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bsTicket */
        /*  */
        THIS-OBJECT:bsTicket:MaxDataGuess = 0.
        THIS-OBJECT:bsTicket:NoLOBs = FALSE.
        THIS-OBJECT:bsTicket:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 46 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("ADDR-1", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("ADDR-2", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("ADDR-3", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("ADDR-4", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("ADDR-5", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("BAL-1", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("BAL-2", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[8] = NEW Progress.Data.ColumnPropDesc("BAL-3", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[9] = NEW Progress.Data.ColumnPropDesc("BAL-4", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[10] = NEW Progress.Data.ColumnPropDesc("C-NAME", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[11] = NEW Progress.Data.ColumnPropDesc("C-STATUS", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[12] = NEW Progress.Data.ColumnPropDesc("C-TYPE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[13] = NEW Progress.Data.ColumnPropDesc("CITY-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[14] = NEW Progress.Data.ColumnPropDesc("CNTY-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[15] = NEW Progress.Data.ColumnPropDesc("COMMENTS", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[16] = NEW Progress.Data.ColumnPropDesc("COMP#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[17] = NEW Progress.Data.ColumnPropDesc("COST-YTD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[18] = NEW Progress.Data.ColumnPropDesc("CST-LAST", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[19] = NEW Progress.Data.ColumnPropDesc("CUST#", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[20] = NEW Progress.Data.ColumnPropDesc("DIV#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[21] = NEW Progress.Data.ColumnPropDesc("EXP-DATE", "null", Progress.Data.DataType:DATE).
        arrayvar1[22] = NEW Progress.Data.ColumnPropDesc("EXPIRE-DT", "null", Progress.Data.DataType:DATE).
        arrayvar1[23] = NEW Progress.Data.ColumnPropDesc("Fuel%", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[24] = NEW Progress.Data.ColumnPropDesc("Fuel_Sur", "null", Progress.Data.DataType:LOGICAL).
        arrayvar1[25] = NEW Progress.Data.ColumnPropDesc("FUTR-DUE", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[26] = NEW Progress.Data.ColumnPropDesc("LP-DATE", "null", Progress.Data.DataType:DATE).
        arrayvar1[27] = NEW Progress.Data.ColumnPropDesc("LST-DATE", "null", Progress.Data.DataType:DATE).
        arrayvar1[28] = NEW Progress.Data.ColumnPropDesc("OPEN-DT", "null", Progress.Data.DataType:DATE).
        arrayvar1[29] = NEW Progress.Data.ColumnPropDesc("PO#", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[30] = NEW Progress.Data.ColumnPropDesc("ROUTE#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[31] = NEW Progress.Data.ColumnPropDesc("SHRT-NAM", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[32] = NEW Progress.Data.ColumnPropDesc("SLS#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[33] = NEW Progress.Data.ColumnPropDesc("SLS-LAST", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[34] = NEW Progress.Data.ColumnPropDesc("SLS-YTD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[35] = NEW Progress.Data.ColumnPropDesc("SRVC-CHG", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[36] = NEW Progress.Data.ColumnPropDesc("SRVC-DAYS", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[37] = NEW Progress.Data.ColumnPropDesc("SRVC-DUE", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[38] = NEW Progress.Data.ColumnPropDesc("SRVC-PER", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[39] = NEW Progress.Data.ColumnPropDesc("SRVC-YTD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[40] = NEW Progress.Data.ColumnPropDesc("STATE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[41] = NEW Progress.Data.ColumnPropDesc("STMT-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[42] = NEW Progress.Data.ColumnPropDesc("SUB#", "null", Progress.Data.DataType:INTEGER).
        arrayvar1[43] = NEW Progress.Data.ColumnPropDesc("TELE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[44] = NEW Progress.Data.ColumnPropDesc("TERM-CD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar1[45] = NEW Progress.Data.ColumnPropDesc("TOT-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar1[46] = NEW Progress.Data.ColumnPropDesc("ZIP", "null", Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar1.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar2 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc3:ChildTables = arrayvar2.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 42 NO-UNDO.
        arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("BudgetedHours", "Budgeted Hrs", Progress.Data.DataType:DECIMAL).
        arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("CABLE", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("CABLE-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[4] = NEW Progress.Data.ColumnPropDesc("COMP#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[5] = NEW Progress.Data.ColumnPropDesc("CUST#", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[6] = NEW Progress.Data.ColumnPropDesc("DATE-PRT", "null", Progress.Data.DataType:DATE).
        arrayvar3[7] = NEW Progress.Data.ColumnPropDesc("DATE-RET", "null", Progress.Data.DataType:DATE).
        arrayvar3[8] = NEW Progress.Data.ColumnPropDesc("DIV#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[9] = NEW Progress.Data.ColumnPropDesc("DL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[10] = NEW Progress.Data.ColumnPropDesc("DL-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[11] = NEW Progress.Data.ColumnPropDesc("DL-BUD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[12] = NEW Progress.Data.ColumnPropDesc("DL-LAST", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[13] = NEW Progress.Data.ColumnPropDesc("FREQ", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[14] = NEW Progress.Data.ColumnPropDesc("ITEM#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[15] = NEW Progress.Data.ColumnPropDesc("M-NAME", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[16] = NEW Progress.Data.ColumnPropDesc("M-NUM", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[17] = NEW Progress.Data.ColumnPropDesc("MONTH#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[18] = NEW Progress.Data.ColumnPropDesc("MTD-VOL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[19] = NEW Progress.Data.ColumnPropDesc("OTH-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[20] = NEW Progress.Data.ColumnPropDesc("OTHER", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[21] = NEW Progress.Data.ColumnPropDesc("PRISEC", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[22] = NEW Progress.Data.ColumnPropDesc("PROPSL#", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[23] = NEW Progress.Data.ColumnPropDesc("PRT", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[24] = NEW Progress.Data.ColumnPropDesc("RET", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[25] = NEW Progress.Data.ColumnPropDesc("ROUTE#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[26] = NEW Progress.Data.ColumnPropDesc("SCAFF-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[27] = NEW Progress.Data.ColumnPropDesc("SCAFFOLD", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[28] = NEW Progress.Data.ColumnPropDesc("SUB#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[29] = NEW Progress.Data.ColumnPropDesc("T-INDX", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[30] = NEW Progress.Data.ColumnPropDesc("T-STAT", "null", Progress.Data.DataType:CHARACTER).
        arrayvar3[31] = NEW Progress.Data.ColumnPropDesc("TicketDate", "Ticket Date", Progress.Data.DataType:DATE).
        arrayvar3[32] = NEW Progress.Data.ColumnPropDesc("TicketDateConfirmed", "Ticket Date Confirmed", Progress.Data.DataType:LOGICAL).
        arrayvar3[33] = NEW Progress.Data.ColumnPropDesc("TOT-AMT", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[34] = NEW Progress.Data.ColumnPropDesc("TOT-AMT-REM", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[35] = NEW Progress.Data.ColumnPropDesc("wk#", "null", Progress.Data.DataType:INTEGER).
        arrayvar3[36] = NEW Progress.Data.ColumnPropDesc("WK-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[37] = NEW Progress.Data.ColumnPropDesc("WK-DL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[38] = NEW Progress.Data.ColumnPropDesc("WK-DL-BAL", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[39] = NEW Progress.Data.ColumnPropDesc("WK-DL-BUD", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[40] = NEW Progress.Data.ColumnPropDesc("WK-START", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[41] = NEW Progress.Data.ColumnPropDesc("WK-USED", "null", Progress.Data.DataType:DECIMAL).
        arrayvar3[42] = NEW Progress.Data.ColumnPropDesc("WTD-VOL", "null", Progress.Data.DataType:DECIMAL).
        tableDesc3:Columns = arrayvar3.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS Progress.Data.TableDesc EXTENT 2 NO-UNDO.
        arrayvar4[1] = tableDesc2.
        arrayvar4[2] = tableDesc3.
        tableDesc1:ChildTables = arrayvar4.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 1 NO-UNDO.
        arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("Field1", "Field1", Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar5.
        THIS-OBJECT:bsTicket:TableSchema = tableDesc1.
        /*  */
        /* Billing */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(574, 498).
        THIS-OBJECT:Name = "Billing".
        THIS-OBJECT:Text = "Billing".
        CAST(THIS-OBJECT:bsTicket, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC Billing ( ):
       IF VALID-HANDLE(qTicket) THEN DO: 
          qTicket:QUERY-CLOSE().
          DELETE OBJECT qTicket.
       END.
    END DESTRUCTOR.

END CLASS.