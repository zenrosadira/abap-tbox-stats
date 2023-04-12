CLASS ztbox_cl_stats DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_floats TYPE TABLE OF f WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_values,
        x TYPE string,
        y TYPE string,
      END OF ty_values .
    TYPES:
      ty_values_t TYPE TABLE OF ty_values WITH DEFAULT KEY .

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !table TYPE ANY TABLE
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS standardize
      IMPORTING
        !col TYPE name_feld DEFAULT `TABLE_LINE`
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS normalize
      IMPORTING
        !col TYPE name_feld DEFAULT `TABLE_LINE`
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS count
      RETURNING
        VALUE(r) TYPE i .
    METHODS count_not_initial
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE i
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS count_distinct
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE i
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS interquartile_range
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS third_quartile
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS second_quartile
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS first_quartile
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS standard_deviation
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS kurtosis
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS skewness
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS variance
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS median
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS quadratic_mean
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS harmonic_mean
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS geometric_mean
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS mean
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS sum
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS max
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS min
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS distinct
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string_table
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS group_by
      IMPORTING
        !grouping TYPE string
      RETURNING
        VALUE(r)  TYPE REF TO ztbox_cl_stats_group
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS outliers
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE string_table
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS histogram
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE ty_values_t
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS empirical_pdf
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE ty_values_t
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS empirical_cdf
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      RETURNING
        VALUE(r) TYPE ty_values_t
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS correlation
      IMPORTING
        !cols    TYPE string
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS covariance
      IMPORTING
        !cols    TYPE string
      RETURNING
        VALUE(r) TYPE string
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    CLASS-METHODS standard
      IMPORTING
        !size    TYPE i DEFAULT 1
      RETURNING
        VALUE(r) TYPE ty_floats .
    CLASS-METHODS normal
      IMPORTING
        !mean     TYPE f DEFAULT 0
        !variance TYPE f DEFAULT 1
        !size     TYPE i DEFAULT 1
      RETURNING
        VALUE(r)  TYPE ty_floats .
    CLASS-METHODS uniform
      IMPORTING
        !low     TYPE f DEFAULT 0
        !high    TYPE f DEFAULT 1
        !size    TYPE i DEFAULT 1
      RETURNING
        VALUE(r) TYPE ty_floats .
    METHODS are_normal
      IMPORTING
        !col     TYPE name_feld DEFAULT `TABLE_LINE`
      EXPORTING
        !p_value TYPE f
      RETURNING
        VALUE(r) TYPE abap_bool
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS col
      IMPORTING
        !col     TYPE name_feld
      RETURNING
        VALUE(r) TYPE REF TO ztbox_cl_stats
      RAISING
        RESUMABLE(zcx_tbox_stats) .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_float_value,
        ix    TYPE i,
        value TYPE f,
      END OF ty_float_value .
  types:
    ty_float_values TYPE TABLE OF ty_float_value WITH DEFAULT KEY .
  types:
    BEGIN OF ty_string_value,
        ix    TYPE i,
        value TYPE string,
      END OF ty_string_value .
  types:
    ty_string_values TYPE TABLE OF ty_string_value WITH DEFAULT KEY .
  types:
    BEGIN OF ty_values_indexed,
        ix    TYPE i,
        value TYPE f,
      END OF ty_values_indexed .
  types:
    ty_values_indexed_t TYPE TABLE OF ty_values_indexed WITH DEFAULT KEY .
  types:
    BEGIN OF ty_num_value,
        column TYPE name_feld,
        elem   TYPE REF TO cl_abap_elemdescr,
        values TYPE ty_float_values,
      END OF ty_num_value .
  types:
    ty_num_values TYPE TABLE OF ty_num_value WITH DEFAULT KEY .
  types:
    BEGIN OF ty_str_value,
        column TYPE name_feld,
        elem   TYPE REF TO cl_abap_elemdescr,
        values TYPE ty_string_values,
      END OF ty_str_value .
  types:
    ty_str_values TYPE TABLE OF ty_str_value WITH DEFAULT KEY .
  types:
    BEGIN OF ty_catalog,
        column TYPE name_feld,
        elem   TYPE REF TO cl_abap_elemdescr,
        values TYPE TABLE OF ty_values_indexed WITH DEFAULT KEY,
      END OF ty_catalog .
  types:
    ty_catalog_t TYPE TABLE OF ty_catalog WITH DEFAULT KEY .

  data _DATA type ref to DATA .
  class-data:
    _numerical_types TYPE RANGE OF abap_typekind .
  data _COMPONENTS type ABAP_COMPONENT_TAB .
  data _NUMERICAL_VALUES type TY_NUM_VALUES .
  data _CATEGORIAL_VALUES type TY_STR_VALUES .
  data _TABLE_LINE type ABAP_BOOL .

  class-methods _SET_NUMERICAL_TYPES .
  methods _CREATE_BINS
    importing
      !COL type NAME_FELD
    returning
      value(R) type TY_VALUES_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _SET_VALUES
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _GET_NUM_VALUES
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_VALUES_INDEXED_T .
  methods _CREATE_TABLE_LIKE
    importing
      !COL type NAME_FELD
    returning
      value(R) type ref to DATA .
  methods _CREATE_DATA_LIKE
    importing
      !COL type NAME_FELD
    returning
      value(R) type ref to DATA .
  class-methods GAMMA
    importing
      !I type F
    returning
      value(R) type F .
  methods _CHECK_COL
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _CHECK_CAT_COL
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _CHECK_NUM_COL
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _SET_COMPONENTS
    importing
      !TABLE type ANY TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
ENDCLASS.



CLASS ZTBOX_CL_STATS IMPLEMENTATION.


  METHOD are_normal.

    _check_num_col( col ).

    DATA(values) = _get_num_values( col ).

    DATA(total) = CONV f( count( ) ).
    DATA(skew)  = CONV f( skewness( col ) ).
    DATA(kurt)  = CONV f( kurtosis( col ) ).

    DATA(jb) = ( ( total + `1.0` ) / `6.0` ) * ( skew ** 2 + ( ( kurt - `0.0` ) ** 2 ) / `4.0` ).

    p_value = exp( ( `-1.0` ) * jb / `2.0` ).

    r = xsdbool( p_value > `0.05` ).

  ENDMETHOD.


  METHOD class_constructor.

    _set_numerical_types( ).

  ENDMETHOD.


  METHOD col.

    _check_num_col( col ).

    DATA(tab_val)   = _create_table_like( col ).
    FIELD-SYMBOLS <sng> TYPE ANY TABLE.
    ASSIGN tab_val->* TO <sng>.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    ASSIGN _data->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT col OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      INSERT <val> INTO TABLE <sng>.
    ENDLOOP.

    r = NEW #( <sng> ).

  ENDMETHOD.


  METHOD constructor.

    _set_components( table ).

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    CREATE DATA _data   LIKE table.

    ASSIGN _data->* TO <tab>.

    <tab>   = table.

    _set_values( ).

  ENDMETHOD.


  METHOD correlation.

    SPLIT cols AT `,` INTO DATA(col_1) DATA(col_2).

    _check_num_col( condense( col_1 ) ).
    _check_num_col( condense( col_2 ) ).

    DATA(std_dev_1) = CONV f( standard_deviation( condense( col_1 ) ) ).
    DATA(std_dev_2) = CONV f( standard_deviation( condense( col_2 ) ) ).

    r = CONV f( covariance( cols ) ) / ( std_dev_1 * std_dev_2 ).

  ENDMETHOD.


  METHOD count.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN _data->* TO <tab>.

    r = lines( <tab> ).

  ENDMETHOD.


  METHOD count_distinct.

    _check_num_col( col ).

    r = lines( distinct( col ) ).

  ENDMETHOD.


  METHOD count_not_initial.

    _check_num_col( col ).

    DATA(values) = _get_num_values( col ).

    DELETE values WHERE value IS INITIAL.

    r = lines( values ).

  ENDMETHOD.


  METHOD covariance.

    SPLIT cols AT `,` INTO DATA(col_1) DATA(col_2).

    _check_num_col( condense( col_1 ) ).
    _check_num_col( condense( col_2 ) ).

    DATA(val_1)   = _get_num_values( condense( col_1 ) ).
    DATA(mean_1)  = CONV f( mean( condense( col_1 ) ) ).
    DATA(val_2)   = _get_num_values( condense( col_2 ) ).
    DATA(mean_2)  = CONV f( mean( condense( col_2 ) ) ).
    DATA(total)   = CONV f( count( ) ).

    r = REDUCE f( INIT co = CONV f( 0 ) FOR i = 1 UNTIL i = total + 1 NEXT co = co + val_1[ i ]-value * val_2[ i ]-value - mean_1 * mean_2 ).

    r = r / ( total - `1` ).

  ENDMETHOD.


  METHOD distinct.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    SORT values BY value.
    DELETE ADJACENT DUPLICATES FROM values COMPARING value.

    LOOP AT values INTO DATA(val).

      <val> = val-value.
      APPEND <val> TO r.

    ENDLOOP.

  ENDMETHOD.


  METHOD empirical_cdf.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(total)       = count( ).
    DATA(dist_values) = distinct( col ).

    r = VALUE #( FOR dv IN dist_values
      ( x = dv
        y = REDUCE f( INIT p = 0 FOR v IN values WHERE ( value LE CONV f( dv ) ) NEXT p = p + 1 ) / total ) ).

  ENDMETHOD.


  METHOD empirical_pdf.

    _check_num_col( col ).

    DATA(hist)  = histogram( col ).
    DATA(n)     = CONV f( count( ) ).

    r = VALUE #( FOR _hist IN hist ( x = _hist-x y = _hist-y / n ) ).

  ENDMETHOD.


  METHOD first_quartile.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(total)     = count( ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    SORT values BY value.

    DATA(left_ix)   = floor( CONV f( ( total + 1 ) / 4 ) ).
    DATA(right_ix)  = ceil( CONV f( ( total + 1 ) / 4 ) ).

    r = <val> = ( values[ left_ix ]-value + values[ right_ix ]-value ) / 2.

  ENDMETHOD.


  METHOD gamma.

    r = sqrt( 2 * acos( -1 ) * i ) * ( ( i / exp( 1 ) ) ** i ) * ( ( i * sinh( 1 / i ) ) ** ( i / 2 ) ).

    r = r * exp( ( 7 / 324 ) * ( 1 / ( ( i ** 3 ) * ( 35 * ( i ** 2 ) + 33 ) ) ) ).

    r = r / i.

  ENDMETHOD.


  METHOD geometric_mean.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(res)     = _create_data_like( col ).
    DATA(lines)   = CONV f( count( ) ).

    ASSIGN res->* TO FIELD-SYMBOL(<val>).
    r = <val> = REDUCE #( INIT gm = CONV f( 1 ) FOR val IN values NEXT gm = gm * ( val-value ** ( 1 / lines ) ) ).

  ENDMETHOD.


  METHOD group_by.

    SPLIT grouping AT `,` INTO TABLE DATA(fields_group).

    r = NEW #(
      data      = _data
      grouping  = fields_group ).

  ENDMETHOD.


  METHOD harmonic_mean.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(res)     = _create_data_like( col ).
    DATA(lines)   = CONV f( count( ) ).

    ASSIGN res->* TO FIELD-SYMBOL(<val>).
    r = <val> = lines / REDUCE #( INIT sr = CONV f( 0 ) FOR val IN values NEXT sr = sr + ( 1 / val-value ) ).

  ENDMETHOD.


  METHOD histogram.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(bins)    = _create_bins( col ).

    r = VALUE #( FOR bin IN bins
      ( x = bin-y
        y = REDUCE f( INIT f = CONV f( 0 ) FOR v IN values WHERE ( value > bin-x AND value <= bin-y ) NEXT f = f + 1 )  ) ).

  ENDMETHOD.


  METHOD interquartile_range.

    r = third_quartile( col ) - first_quartile( col ).

  ENDMETHOD.


  METHOD kurtosis.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(std_dev)     = CONV f( standard_deviation( col ) ).
    DATA(mean_value)  = CONV f( mean( col ) ).
    DATA(total)       = CONV f( count( ) ).

    r = REDUCE #( INIT v = CONV f( 0 ) FOR val IN values NEXT v = v + ( ( val-value - mean_value ) / std_dev ) ** 4 ).

    r = r * ( total * ( total + CONV f( 1 ) ) ) / ( ( total - CONV f( 1 ) ) * ( total - CONV f( 2 ) ) * ( total - CONV f( 3 ) ) ).

    r = r - 3 * ( total - CONV f( 1 ) ) ** 2 / ( ( total - CONV f( 2 ) ) * ( total - CONV f( 3 ) ) ).

  ENDMETHOD.


  METHOD max.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    SORT values BY value DESCENDING.
    r = <val> = values[ 1 ]-value.

  ENDMETHOD.


  METHOD mean.

    _check_num_col( col ).

    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    r = <val> = CONV f( sum( col ) ) / CONV f( count( ) ).

  ENDMETHOD.


  METHOD median.

    _check_num_col( col ).

    r = second_quartile( col ).

  ENDMETHOD.


  METHOD min.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    SORT values BY value.
    r = <val> = values[ 1 ]-value.

  ENDMETHOD.


  METHOD normal.

    DATA(s) = standard( size ).

    r = VALUE #( FOR x IN s ( x * variance + mean ) ).

  ENDMETHOD.


  METHOD normalize.

    _check_num_col( col ).

    DATA(min) = CONV f( min( col ) ).
    DATA(max) = CONV f( max( col ) ).

    READ TABLE _numerical_values ASSIGNING FIELD-SYMBOL(<cat>) WITH KEY column = col.

    LOOP AT <cat>-values ASSIGNING FIELD-SYMBOL(<val>).
      <val>-value = ( <val>-value - min ) / ( max - min ).
    ENDLOOP.

  ENDMETHOD.


  METHOD outliers.

    _check_num_col( col ).

    DATA(dist_values)     = distinct( col ).
    DATA(first_quartile)  = CONV f( first_quartile( col ) ).
    DATA(third_quartile)  = CONV f( third_quartile( col ) ).
    DATA(iqr)             = third_quartile - first_quartile.

    LOOP AT dist_values INTO DATA(value).

      IF CONV f( value ) < ( first_quartile - ( 3 * iqr / 2 ) ) OR
         CONV f( value ) > ( third_quartile + ( 3 * iqr / 2 ) ).

        APPEND value TO r.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD quadratic_mean.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(res)     = _create_data_like( col ).
    DATA(total)   = CONV f( count( ) ).

    ASSIGN res->* TO FIELD-SYMBOL(<val>).
    r = REDUCE #( INIT sr = CONV f( 0 ) FOR val IN values NEXT sr = sr + ( val-value ) ** 2 ).

    r = <val> = sqrt( r / total ).

  ENDMETHOD.


  METHOD second_quartile.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(total)     = count( ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    SORT values BY value.

    DATA(left_ix)   = floor( CONV f( ( total + 1 ) / 2 ) ).
    DATA(right_ix)  = ceil( CONV f( ( total + 1 ) / 2 ) ).

    r = <val> = ( values[ left_ix ]-value + values[ right_ix ]-value ) / 2.

  ENDMETHOD.


  METHOD skewness.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(std_dev)     = CONV f( standard_deviation( col ) ).
    DATA(mean_value)  = CONV f( mean( col ) ).
    DATA(total)       = CONV f( count( ) ).

    r = REDUCE #( INIT v = CONV f( 0 ) FOR val IN values NEXT v = v + ( ( val-value - mean_value ) / std_dev ) ** 3 ).

    r = r * total / ( ( total - CONV f( 1 ) ) * ( total - CONV f( 2 ) ) ).

  ENDMETHOD.


  METHOD standard.

    DATA(i) = SWITCH i( size MOD 2 WHEN 0 THEN size / 2 ELSE ( size + 1 ) / 2 ).

    DO i TIMES.

      DATA(u_1) = uniform( ).
      DATA(u_2) = uniform( ).

      APPEND ( sqrt( ( -2 ) * log( CONV f( u_1[ 1 ] ) ) ) * cos( 2 * acos( -1 ) * CONV f( u_2[ 1 ] ) ) ) TO r.

      IF sy-index EQ i AND i MOD 2 EQ 0.
        APPEND ( sqrt( ( -2 ) * log( CONV f( u_1[ 1 ] ) ) ) * sin( 2 * acos( -1 ) * CONV f( u_2[ 1 ] ) ) ) TO r.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD standardize.

    _check_num_col( col ).

    DATA(mean)    = CONV f( mean( col ) ).
    DATA(std_dev) = CONV f( standard_deviation( col ) ).

    READ TABLE _numerical_values ASSIGNING FIELD-SYMBOL(<cat>) WITH KEY column = col.

    LOOP AT <cat>-values ASSIGNING FIELD-SYMBOL(<val>).
      <val>-value = ( <val>-value - mean ) / std_dev.
    ENDLOOP.

  ENDMETHOD.


  METHOD standard_deviation.

    _check_num_col( col ).

    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    r = <val> = sqrt( CONV f( variance( col ) ) ).

  ENDMETHOD.


  METHOD sum.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    DATA(res) = REDUCE #( INIT sum = CONV f( 0 ) FOR val IN values NEXT sum = sum + val-value ).

    TRY.

        r = <val> = res.

      CATCH cx_sy_conversion_overflow.

        r = res.

    ENDTRY.

  ENDMETHOD.


  METHOD third_quartile.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(total)     = count( ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    SORT values BY value.

    DATA(left_ix)   = floor( CONV f( ( 3 * ( total + 1 ) ) / 4 ) ).
    DATA(right_ix)  = ceil( CONV f( ( 3 * ( total + 1 ) ) / 4 ) ).

    r = <val> = ( values[ left_ix ]-value + values[ right_ix ]-value ) / 2.

  ENDMETHOD.


  METHOD uniform.

    DATA(randomizer) = cl_abap_random_float=>create( cl_abap_random=>seed( ) ).

    DO size TIMES.

      DATA(rand_float) = randomizer->get_next( ).

      APPEND ( low + ( high - low ) * rand_float ) TO r.

    ENDDO.

  ENDMETHOD.


  METHOD variance.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(mean_value)  = CONV f( mean( col ) ).
    DATA(total)       = CONV f( count( ) ).
    DATA(value_ref)   = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    DATA(var) = REDUCE #( INIT v = CONV f( 0 ) FOR val IN values NEXT v = v + ( val-value - mean_value ) ** 2 ) / ( total - 1 ).

    TRY.

        r = <val> = var.

      CATCH cx_sy_conversion_overflow.

        r = var.

    ENDTRY.

  ENDMETHOD.


  METHOD _check_cat_col.

    IF NOT line_exists( _categorial_values[ column = col ] ).
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>column_not_categorical.
    ENDIF.

  ENDMETHOD.


  METHOD _check_col.

    IF NOT line_exists( _components[ name = col ] ).
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>column_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD _check_num_col.

    IF NOT line_exists( _numerical_values[ column = col ] ).
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>column_not_numerical.
    ENDIF.

  ENDMETHOD.


  METHOD _create_bins.

    DATA(values)  = distinct( col ).

    DATA(diff)    = VALUE string_table( ).

    DATA prev_value TYPE f.
    DATA bin_len    TYPE f.

    LOOP AT values INTO DATA(value).
      bin_len = bin_len + CONV f( value ) - prev_value.
      prev_value = CONV f( value ).
    ENDLOOP.

    bin_len = ( bin_len / lines( values ) ) * 2.

    DATA(min_val) = VALUE f( values[ 1 ] OPTIONAL ).
    DATA(max_val) = VALUE f( values[ lines( values ) ] OPTIONAL ).

    APPEND VALUE #( x = min_val - bin_len y = min_val ) TO r.

    WHILE min_val < max_val + bin_len.
      APPEND VALUE #( x = min_val y = min_val + bin_len ) TO r.
      min_val = min_val + bin_len.
    ENDWHILE.

  ENDMETHOD.


  METHOD _create_data_like.

    DATA(elem) = CAST cl_abap_elemdescr( _components[ name = col ]-type ).

    CREATE DATA r TYPE HANDLE elem.

  ENDMETHOD.


  METHOD _create_table_like.

    DATA(elem) = CAST cl_abap_elemdescr( _components[ name = col ]-type ).

    DATA(table) = cl_abap_tabledescr=>create( elem ).

    CREATE DATA r TYPE HANDLE table.

  ENDMETHOD.


  METHOD _get_num_values.

    r = _numerical_values[ column = col ]-values.

  ENDMETHOD.


  METHOD _set_components.

    DATA(tab_type)  = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) ).
    DATA(line_type) = tab_type->get_table_line_type( ).

    CASE line_type->kind.

      WHEN cl_abap_typedescr=>kind_struct.
        DATA(str_type)    = CAST cl_abap_structdescr( line_type ).
        DATA(components)  = str_type->components.

        _components = VALUE #( FOR comp IN components ( name = comp-name type = str_type->get_component_type( comp-name ) ) ).

      WHEN cl_abap_typedescr=>kind_elem.
        DATA(elem)        = CAST cl_abap_elemdescr( line_type ).

        _components = VALUE #( ( name = `TABLE_LINE` type = elem ) ).
        _table_line = abap_true.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>table_type_not_supported.

    ENDCASE.

    _numerical_values   = VALUE #( FOR _nc IN _components WHERE ( type->type_kind IN _numerical_types )
      ( column = _nc-name elem = CAST cl_abap_elemdescr( _nc-type ) ) ).
    _categorial_values  = VALUE #( FOR _sc IN _components WHERE ( type->type_kind NOT IN _numerical_types )
      ( column = _sc-name elem = CAST cl_abap_elemdescr( _sc-type ) ) ).

  ENDMETHOD.


  METHOD _set_numerical_types.

    _numerical_types = VALUE #(
      sign    = if_fsbp_const_range=>sign_include
      option  = if_fsbp_const_range=>option_equal
      ( low = cl_abap_typedescr=>typekind_int )
      ( low = cl_abap_typedescr=>typekind_int1 )
      ( low = cl_abap_typedescr=>typekind_int2 )
      ( low = cl_abap_typedescr=>typekind_int8 )
      ( low = cl_abap_typedescr=>typekind_packed )
      ( low = cl_abap_typedescr=>typekind_decfloat )
      ( low = cl_abap_typedescr=>typekind_decfloat16 )
      ( low = cl_abap_typedescr=>typekind_decfloat34 )
      ( low = cl_abap_typedescr=>typekind_float ) ).

  ENDMETHOD.


  METHOD _set_values.

    FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN _data->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      DATA(ix) = sy-tabix.

      IF _table_line EQ abap_true.
        ASSIGN <row> TO <val>.
      ENDIF.

      LOOP AT _numerical_values ASSIGNING FIELD-SYMBOL(<num>).
        IF _table_line EQ abap_false.
          ASSIGN COMPONENT <num>-column OF STRUCTURE <row> TO <val>.
        ENDIF.
        APPEND VALUE #( ix = ix value = CONV f( <val> ) ) TO <num>-values.
      ENDLOOP.

      LOOP AT _categorial_values ASSIGNING FIELD-SYMBOL(<cat>).
        IF _table_line EQ abap_false.
          ASSIGN COMPONENT <cat>-column OF STRUCTURE <row> TO <val>.
        ENDIF.
        APPEND VALUE #( ix = ix value = CONV string( <val> ) ) TO <cat>-values.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
