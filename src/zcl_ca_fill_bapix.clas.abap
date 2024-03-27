CLASS zcl_ca_fill_bapix DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data</p>
    "!
    "! @parameter bapi_data       | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax      | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for ZCL_CA_FILL_BAPIX</p>
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
    "! @parameter bapi_data       | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax      | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for ZCL_CA_FILL_BAPIX</p>
    CLASS-METHODS _fill_bapix_structure
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data
      RAISING
        zcx_ca_fill_bapix.

    "! <p class="shorttext synchronized">Fill BAPIX data table dependent on BAPI data table</p>
    "!
    "! @parameter bapi_data       | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax      | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for ZCL_CA_FILL_BAPIX</p>
    CLASS-METHODS _fill_bapix_table
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data
      RAISING
        zcx_ca_fill_bapix.

    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data using data dictionary</p>
    "!
    "! @parameter bapi_data       | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax      | <p class="shorttext synchronized">BAPIX data</p>
    CLASS-METHODS _fill_bapix_structure_by_ddic
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data.

    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data using components</p>
    "!
    "! @parameter bapi_data       | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax      | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for ZCL_CA_FILL_BAPIX</p>
    CLASS-METHODS _fill_bapix_structure_by_comp
      IMPORTING
        bapi_data  TYPE data
      CHANGING
        bapi_datax TYPE data
      RAISING
        zcx_ca_fill_bapix.
ENDCLASS.



CLASS zcl_ca_fill_bapix IMPLEMENTATION.
  METHOD fill_bapix.
    CASE TYPE OF cl_abap_typedescr=>describe_by_data( bapi_data ).
      WHEN TYPE cl_abap_structdescr INTO DATA(bapi_data_structdescr).
        IF NOT cl_abap_typedescr=>describe_by_data( bapi_datax ) IS INSTANCE OF cl_abap_structdescr.
          RAISE EXCEPTION TYPE zcx_ca_fill_bapix
            EXPORTING
              textid         = zcx_ca_fill_bapix=>parameter_wrong_type
              parameter_name = 'BAPI_DATAX'.

        ENDIF.
      WHEN TYPE cl_abap_tabledescr INTO DATA(bapi_data_tabledescr).
        bapi_data_structdescr = COND #( WHEN bapi_data_tabledescr->get_table_line_type( ) IS INSTANCE OF cl_abap_structdescr
                                        THEN CAST #( bapi_data_tabledescr->get_table_line_type( ) ) ).
        IF NOT cl_abap_typedescr=>describe_by_data( bapi_datax ) IS INSTANCE OF cl_abap_structdescr.
          RAISE EXCEPTION TYPE zcx_ca_fill_bapix
            EXPORTING
              textid         = zcx_ca_fill_bapix=>parameter_wrong_type
              parameter_name = 'BAPI_DATAX'.

        ENDIF.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>parameter_wrong_type
            parameter_name = 'BAPI_DATA'.
    ENDCASE.
    IF bapi_data_tabledescr IS BOUND.
      _fill_bapix_table(
        EXPORTING
          bapi_data  = bapi_data
        CHANGING
          bapi_datax = bapi_datax ).
    ELSE.
      _fill_bapix_structure(
        EXPORTING
          bapi_data  = bapi_data
        CHANGING
          bapi_datax = bapi_datax ).
    ENDIF.
  ENDMETHOD.

  METHOD _fill_bapix_structure.
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
    FIELD-SYMBOLS <bapi_data> TYPE STANDARD TABLE.
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

    LOOP AT bapi_data_components REFERENCE INTO DATA(bapi_data_component).
      IF bapi_data_component->type IS NOT INSTANCE OF cl_abap_elemdescr.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATA'.
      ENDIF.
    ENDLOOP.
    LOOP AT bapi_datax_components REFERENCE INTO DATA(bapi_datax_component).
      IF bapi_datax_component->type IS NOT INSTANCE OF cl_abap_elemdescr.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATAX'.
      ENDIF.
    ENDLOOP.

    LOOP AT bapi_datax_components REFERENCE INTO bapi_datax_component.
      bapi_data_component = REF #( bapi_data_components[ name = bapi_datax_component->name ] OPTIONAL ).
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
ENDCLASS.