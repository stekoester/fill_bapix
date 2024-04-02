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

    CLASS-METHODS _is_flat
      IMPORTING
        abap_structdescr TYPE REF TO cl_abap_structdescr
      RETURNING
        value(is_flat)   TYPE abap_bool.
ENDCLASS.


CLASS zcl_ca_fill_bapix IMPLEMENTATION.
  METHOD fill_bapix.
    IF bapi_data IS INITIAL.
      RETURN.
    ENDIF.

    DATA(parameter_kind) = _get_parameter_kind( bapi_data  = bapi_data
                                                bapi_datax = bapi_datax ).

    CASE parameter_kind.
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
    DATA(bapi_data_components) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( bapi_data ) )->get_components( ).
    DATA(bapi_datax_components) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( bapi_datax ) )->get_components( ).

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
    CASE TYPE OF bapi_data_typedescr.
      WHEN TYPE cl_abap_structdescr INTO DATA(bapi_data_structdescr).
        IF NOT _is_flat( bapi_data_structdescr ).
          RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
                                                 parameter_name = 'BAPI_DATA' ).

        ENDIF.
        IF cl_abap_typedescr=>describe_by_data( bapi_datax ) IS NOT INSTANCE OF cl_abap_structdescr.
          RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>parameter_wrong_type
                                                 parameter_name = 'BAPI_DATAX' ).
        ELSEIF cl_abap_typedescr=>describe_by_data( bapi_datax ) IS INSTANCE OF cl_abap_structdescr AND NOT _is_flat( CAST #( cl_abap_typedescr=>describe_by_data( bapi_datax ) ) ).
          RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
                                                 parameter_name = 'BAPI_DATAX' ).

        ENDIF.
      WHEN TYPE cl_abap_tabledescr INTO DATA(bapi_data_tabledescr).
        bapi_data_structdescr = COND #( WHEN bapi_data_tabledescr->get_table_line_type( ) IS INSTANCE OF cl_abap_structdescr
                                        THEN CAST #( bapi_data_tabledescr->get_table_line_type( ) ) ).
        IF bapi_data_structdescr IS BOUND AND NOT _is_flat( bapi_data_structdescr ).
          RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
                                                 parameter_name = 'BAPI_DATA' ).

        ENDIF.
        CASE TYPE OF cl_abap_typedescr=>describe_by_data( bapi_datax ).
          WHEN TYPE cl_abap_structdescr INTO DATA(bapi_datax_structdescr).
            RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid            = zcx_ca_fill_bapix=>different_types
                                                   parameter_name_01 = 'BAPI_DATA'
                                                   parameter_name_02 = 'BAPI_DATAX' ).
          WHEN TYPE cl_abap_tabledescr INTO DATA(bapi_datax_tabledescr).
            bapi_datax_structdescr = COND #( WHEN bapi_datax_tabledescr->get_table_line_type( ) IS INSTANCE OF cl_abap_structdescr
                                             THEN CAST #( bapi_datax_tabledescr->get_table_line_type( ) ) ).
            IF bapi_datax_structdescr IS NOT BOUND.
              RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>parameter_wrong_type
                                                     parameter_name = 'BAPI_DATAX' ).
            ELSEIF NOT _is_flat( bapi_datax_structdescr ).
              RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
                                                     parameter_name = 'BAPI_DATAX' ).
            ENDIF.
          WHEN OTHERS.
            RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>parameter_wrong_type
                                                   parameter_name = 'BAPI_DATAX' ).
        ENDCASE.
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_ca_fill_bapix( textid         = zcx_ca_fill_bapix=>parameter_wrong_type
                                               parameter_name = 'BAPI_DATA' ).
    ENDCASE.
    parameter_kind = bapi_data_typedescr->kind.
  ENDMETHOD.

  METHOD _is_flat.
    IF abap_structdescr IS BOUND.
      is_flat = abap_true.
      DATA(components) = abap_structdescr->get_components( ).
      LOOP AT components REFERENCE INTO DATA(bapi_data_component).
        IF bapi_data_component->type IS NOT INSTANCE OF cl_abap_elemdescr.
          CLEAR is_flat.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
