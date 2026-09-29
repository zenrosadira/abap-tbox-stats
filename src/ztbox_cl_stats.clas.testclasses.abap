CLASS ltcl_stats DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS histogram_with_empty_bins FOR TESTING RAISING cx_static_check.
    METHODS histogram_of_a_column FOR TESTING RAISING cx_static_check.
    METHODS histogram_with_zero_iqr FOR TESTING RAISING cx_static_check.
    METHODS histogram_of_equal_values FOR TESTING RAISING cx_static_check.
    METHODS standard_returns_size FOR TESTING RAISING cx_static_check.

    CLASS-METHODS total
      IMPORTING
        histogram TYPE ztbox_cl_stats=>ty_hist_t
      RETURNING
        VALUE(r)  TYPE i.

ENDCLASS.


CLASS ltcl_stats IMPLEMENTATION.


  METHOD histogram_with_empty_bins.

    " 0.0 .. 9.9 and five times 100: every bin between the two groups is
    " empty, the five values belong into the last bin
    DATA(values) = VALUE ztbox_cl_stats=>ty_floats( FOR i = 0 UNTIL i = 100 ( CONV f( i ) / 10 ) ).
    values = VALUE #( BASE values FOR j = 1 UNTIL j > 5 ( CONV f( 100 ) ) ).

    DATA(histogram) = NEW ztbox_cl_stats( values )->histogram( ).

    cl_abap_unit_assert=>assert_equals( act = histogram[ lines( histogram ) ]-y exp = 5 ).
    cl_abap_unit_assert=>assert_equals( act = total( histogram ) exp = 105 ).

  ENDMETHOD.


  METHOD histogram_of_a_column.

    TYPES:
      BEGIN OF ty_row,
        id    TYPE i,
        price TYPE p LENGTH 8 DECIMALS 2,
      END OF ty_row.
    DATA rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.

    rows = VALUE #( FOR i = 1 UNTIL i > 50 ( id = i price = i * 2 ) ).

    DATA(histogram) = NEW ztbox_cl_stats( rows )->histogram( `PRICE` ).

    cl_abap_unit_assert=>assert_equals( act = total( histogram ) exp = 50 ).

  ENDMETHOD.


  METHOD histogram_with_zero_iqr.

    " one 0 and nine 1: Q1 = Q3 = 1, so the Freedman-Diaconis width is 0
    DATA(values) = VALUE ztbox_cl_stats=>ty_ints( ( 0 ) ( 1 ) ( 1 ) ( 1 ) ( 1 ) ( 1 ) ( 1 ) ( 1 ) ( 1 ) ( 1 ) ).

    DATA(histogram) = NEW ztbox_cl_stats( values )->histogram( ).

    cl_abap_unit_assert=>assert_equals( act = total( histogram ) exp = 10 ).
    cl_abap_unit_assert=>assert_equals( act = histogram[ 1 ]-y exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = histogram[ lines( histogram ) ]-y exp = 9 ).

  ENDMETHOD.


  METHOD histogram_of_equal_values.

    DATA(values) = VALUE ztbox_cl_stats=>ty_ints( ( 5 ) ( 5 ) ( 5 ) ).

    DATA(histogram) = NEW ztbox_cl_stats( values )->histogram( ).

    cl_abap_unit_assert=>assert_equals( act = lines( histogram ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = total( histogram ) exp = 3 ).

  ENDMETHOD.


  METHOD standard_returns_size.

    cl_abap_unit_assert=>assert_equals( act = lines( ztbox_cl_stats=>standard( size = 10 ) ) exp = 10 ).
    cl_abap_unit_assert=>assert_equals( act = lines( ztbox_cl_stats=>standard( size = 7 ) ) exp = 7 ).
    cl_abap_unit_assert=>assert_equals( act = lines( ztbox_cl_stats=>normal( size = 1000 ) ) exp = 1000 ).

  ENDMETHOD.


  METHOD total.

    r = REDUCE #( INIT sum = 0 FOR bin IN histogram NEXT sum = sum + bin-y ).

  ENDMETHOD.

ENDCLASS.
