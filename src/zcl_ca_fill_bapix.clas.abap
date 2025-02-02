CLASS zcl_ca_fill_bapix DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data</p>
    "!
    "! @parameter bapi_data         | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax        | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising   zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for zcx_ca_fill_bapix</p>
    CLASS-METHODS fill_bapix
      IMPORTING
        bapi_data         TYPE data
      CHANGING
        VALUE(bapi_datax) TYPE data
      RAISING
        zcx_ca_fill_bapix.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Fill BAPIX data structure dependent on BAPI data structure</p>
    "!
    "! @parameter bapi_data         | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax        | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising   zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for zcx_ca_fill_bapix</p>
    CLASS-METHODS _fill_bapix_structure
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data
      RAISING
        zcx_ca_fill_bapix.

    "! <p class="shorttext synchronized">Fill BAPIX data table dependent on BAPI data table</p>
    "!
    "! @parameter bapi_data         | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax        | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising   zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for zcx_ca_fill_bapix</p>
    CLASS-METHODS _fill_bapix_table
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data
      RAISING
        zcx_ca_fill_bapix.

    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data using data dictionary</p>
    "!
    "! @parameter bapi_data  | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax | <p class="shorttext synchronized">BAPIX data</p>
    CLASS-METHODS _fill_bapix_structure_by_ddic
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data.

    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data using components</p>
    "!
    "! @parameter bapi_data         | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax        | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising   zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for zcx_ca_fill_bapix</p>
    CLASS-METHODS _fill_bapix_structure_by_comp
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data
      RAISING
        zcx_ca_fill_bapix.

    "! <p class="shorttext synchronized">Get kind of parameter</p>
    "!
    "! @parameter bapi_data         | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax        | <p class="shorttext synchronized">BAPIX data</p>
    "! @parameter parameter_kind    | <p class="shorttext synchronized">Kind of parameter</p>
    "! @raising   zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for zcx_ca_fill_bapix</p>
    CLASS-METHODS _get_parameter_kind
      IMPORTING
        bapi_data             TYPE data
        bapi_datax            TYPE data
      RETURNING
        VALUE(parameter_kind) TYPE abap_typecategory
      RAISING
        zcx_ca_fill_bapix.

    "! <p class="shorttext synchronized">Is structure flat</p>
    "!
    "! @parameter abap_structdescr | <p class="shorttext synchronized">Structure description</p>
    "! @parameter is_flat          | <p class="shorttext synchronized">Structure is flat</p>
    CLASS-METHODS _is_flat
      IMPORTING
        abap_structdescr TYPE REF TO cl_abap_structdescr
      RETURNING
        VALUE(is_flat)   TYPE abap_bool.

    "! <p class="shorttext synchronized">Get components of type description</p>
    "!
    "! @parameter abap_typedescr | <p class="shorttext synchronized">Type description</p>
    "! @parameter components     | <p class="shorttext synchronized">Components</p>
    CLASS-METHODS _get_components
      IMPORTING
        abap_typedescr    TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(components) TYPE abap_component_tab.
ENDCLASS.


CLASS zcl_ca_fill_bapix IMPLEMENTATION.
  METHOD fill_bapix.
    IF bapi_data IS INITIAL.
      RETURN.
    ENDIF.

    CASE _get_parameter_kind( bapi_data  = bapi_data
                              bapi_datax = bapi_datax ).
      WHEN cl_abap_typedescr=>kind_struct.
        _fill_bapix_structure(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      WHEN cl_abap_typedescr=>kind_table.
        _fill_bapix_table(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
    ENDCASE.
  ENDMETHOD.

  METHOD _fill_bapix_structure.
    IF bapi_data IS INITIAL.
      RETURN.
    ENDIF.
    IF cl_abap_structdescr=>describe_by_data( bapi_data )->is_ddic_type( ) AND
       cl_abap_structdescr=>describe_by_data( bapi_datax )->is_ddic_type( ).
      _fill_bapix_structure_by_ddic(
        EXPORTING
          bapi_data  = bapi_data
        CHANGING
          bapi_datax = bapi_datax ).
    ELSE.
      _fill_bapix_structure_by_comp(
        EXPORTING
          bapi_data  = bapi_data
        CHANGING
          bapi_datax = bapi_datax ).
    ENDIF.
  ENDMETHOD.

  METHOD _fill_bapix_table.
    FIELD-SYMBOLS <bapi_data>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <bapi_datax> TYPE STANDARD TABLE.

    ASSIGN bapi_data TO <bapi_data>.
    ASSIGN bapi_datax TO <bapi_datax>.

    LOOP AT <bapi_data> ASSIGNING FIELD-SYMBOL(<bapi_data_line>).
      APPEND INITIAL LINE TO <bapi_datax> ASSIGNING FIELD-SYMBOL(<bapi_datax_line>).
      _fill_bapix_structure(
        EXPORTING
          bapi_data  = <bapi_data_line>
        CHANGING
          bapi_datax = <bapi_datax_line> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD _fill_bapix_structure_by_ddic.
    DATA(bapi_data_ddic_fields) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( bapi_data ) )->get_ddic_field_list(
                                     p_including_substructres = abap_true ).
    DATA(bapi_datax_ddic_fields) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( bapi_datax ) )->get_ddic_field_list(
                                     p_including_substructres = abap_true ).
    LOOP AT bapi_datax_ddic_fields REFERENCE INTO DATA(datax_ddic_field).
      DATA(data_ddic_field) = REF #( bapi_data_ddic_fields[ fieldname = datax_ddic_field->fieldname ] OPTIONAL ).
      IF data_ddic_field IS NOT BOUND.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT data_ddic_field->fieldname OF STRUCTURE bapi_data
             TO FIELD-SYMBOL(<bapi_data_value>).
      ASSIGN COMPONENT datax_ddic_field->fieldname OF STRUCTURE bapi_datax
             TO FIELD-SYMBOL(<bapi_datax_value>).
      IF data_ddic_field->rollname EQ datax_ddic_field->rollname.
        <bapi_datax_value> = <bapi_data_value>.
      ELSE.
        IF <bapi_data_value> IS NOT INITIAL.
          <bapi_datax_value> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _fill_bapix_structure_by_comp.
    DATA(bapi_data_components) = _get_components( cl_abap_typedescr=>describe_by_data( bapi_data ) ).
    DATA(bapi_datax_components) = _get_components( cl_abap_typedescr=>describe_by_data( bapi_datax ) ).

    LOOP AT bapi_datax_components REFERENCE INTO DATA(bapi_datax_component).
      DATA(bapi_data_component) = REF #( bapi_data_components[ name = bapi_datax_component->name ] OPTIONAL ).
      IF bapi_data_component IS NOT BOUND.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT bapi_data_component->name OF STRUCTURE bapi_data
             TO FIELD-SYMBOL(<bapi_data_value>).
      ASSIGN COMPONENT bapi_datax_component->name OF STRUCTURE bapi_datax
             TO FIELD-SYMBOL(<bapi_datax_value>).
      IF bapi_data_component->type->absolute_name EQ bapi_datax_component->type->absolute_name.
        <bapi_datax_value> = <bapi_data_value>.
      ELSE.
        IF <bapi_data_value> IS NOT INITIAL.
          <bapi_datax_value> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_parameter_kind.
    DATA(bapi_data_typedescr) = cl_abap_typedescr=>describe_by_data( bapi_data ).
    DATA(bapi_datax_typedescr) = cl_abap_typedescr=>describe_by_data( bapi_datax ).
    IF bapi_data_typedescr->kind NA |{ cl_abap_typedescr=>kind_struct }{ cl_abap_typedescr=>kind_table }|.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>parameter_wrong_type
          parameter_name = 'BAPI_DATA'.
    ELSEIF bapi_datax_typedescr->kind NA |{ cl_abap_typedescr=>kind_struct }{ cl_abap_typedescr=>kind_table }|.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>parameter_wrong_type
          parameter_name = 'BAPI_DATAX'.
    ELSEIF bapi_data_typedescr->kind NE bapi_datax_typedescr->kind.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid            = zcx_ca_fill_bapix=>different_types
          parameter_name_01 = 'BAPI_DATA'
          parameter_name_02 = 'BAPI_DATAX'.
    ENDIF.

    IF bapi_data_typedescr->kind EQ cl_abap_typedescr=>kind_struct.
      DATA(bapi_data_structdescr) = CAST cl_abap_structdescr( bapi_data_typedescr ).
      DATA(bapi_datax_structdescr) = CAST cl_abap_structdescr( bapi_datax_typedescr ).
    ELSE.
      DATA(bapi_data_tabledescr) = CAST cl_abap_tabledescr( bapi_data_typedescr ).
      DATA(bapi_datax_tabledescr) = CAST cl_abap_tabledescr( bapi_datax_typedescr ).
      TRY.
          bapi_data_structdescr = CAST #( CAST cl_abap_tabledescr( bapi_data_typedescr )->get_table_line_type( ) ).
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE zcx_ca_fill_bapix
            EXPORTING
              textid         = zcx_ca_fill_bapix=>parameter_wrong_type
              parameter_name = 'BAPI_DATA'.
      ENDTRY.
      TRY.
          bapi_datax_structdescr = CAST #( CAST cl_abap_tabledescr( bapi_datax_typedescr )->get_table_line_type( ) ).
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE zcx_ca_fill_bapix
            EXPORTING
              textid         = zcx_ca_fill_bapix=>parameter_wrong_type
              parameter_name = 'BAPI_DATAX'.
      ENDTRY.
    ENDIF.
    IF NOT _is_flat( bapi_data_structdescr ).
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
          parameter_name = 'BAPI_DATA'.
    ENDIF.
    IF NOT _is_flat( bapi_datax_structdescr ).
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
          parameter_name = 'BAPI_DATAX'.
    ENDIF.
    parameter_kind = bapi_data_typedescr->kind.
  ENDMETHOD.

  METHOD _is_flat.
    IF abap_structdescr IS BOUND.
      is_flat = abap_true.
      DATA(components) = abap_structdescr->get_components( ).
      LOOP AT components REFERENCE INTO DATA(bapi_data_component).
        IF bapi_data_component->as_include EQ abap_true.
          is_flat = _is_flat( CAST #( bapi_data_component->type ) ).
        ELSEIF bapi_data_component->type IS NOT INSTANCE OF cl_abap_elemdescr.
          CLEAR is_flat.
        ENDIF.
        IF is_flat EQ abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD _get_components.
    IF abap_typedescr->kind NE cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.

    components = CAST cl_abap_structdescr( abap_typedescr )->get_components( ).

    LOOP AT components REFERENCE INTO DATA(component)
         WHERE as_include EQ abap_true.
      DATA(insert_position) = sy-tabix.
      INSERT LINES OF _get_components( component->type )
             INTO components
             INDEX insert_position.
    ENDLOOP.
    DELETE components WHERE as_include EQ abap_true.
  ENDMETHOD.
ENDCLASS.
