**********************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2023 Marco Marrone
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
**********************************************************************
class ZTBOX_CL_STATS definition
  public
  final
  create public .

public section.

  types:
    ty_ints TYPE TABLE OF i WITH DEFAULT KEY .
  types:
    ty_floats TYPE TABLE OF f WITH DEFAULT KEY .
  types:
    BEGIN OF ty_values,
        x TYPE string,
        y TYPE string,
      END OF ty_values .
  types:
    ty_values_t TYPE TABLE OF ty_values WITH DEFAULT KEY .
  types:
    BEGIN OF ty_edist,
        x TYPE string,
        y TYPE f,
      END OF ty_edist .
  types:
    ty_edist_t TYPE TABLE OF ty_edist WITH DEFAULT KEY .
  types:
    BEGIN OF ty_bin,
        x TYPE f,
        y TYPE f,
      END OF ty_bin .
  types:
    ty_bins TYPE TABLE OF ty_bin WITH DEFAULT KEY .
  types:
    BEGIN OF ty_hist,
        x TYPE string,
        y TYPE i,
      END OF ty_hist .
  types:
    ty_hist_t TYPE TABLE OF ty_hist WITH DEFAULT KEY .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !TABLE type ANY TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods STANDARDIZE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_FLOATS
    raising
      resumable(ZCX_TBOX_STATS) .
  methods NORMALIZE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_FLOATS
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COUNT
    returning
      value(R) type I .
  methods COUNT_NOT_INITIAL
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type I
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COUNT_DISTINCT
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type I
    raising
      resumable(ZCX_TBOX_STATS) .
  methods INTERQUARTILE_RANGE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods THIRD_QUARTILE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods SECOND_QUARTILE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods FIRST_QUARTILE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods DISPERSION_INDEX
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COEFFICIENT_VARIATION
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods STANDARD_DEVIATION
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods KURTOSIS
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods SKEWNESS
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods VARIANCE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MEDIAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods QUADRATIC_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods HARMONIC_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods GEOMETRIC_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MAD_MEDIAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MAD_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods SUM
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MAX
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods RANGE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MIN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _NUM_DISTINCT
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING_TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _CAT_DISTINCT
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING_TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods DISTINCT
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING_TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods GROUP_BY
    importing
      !GROUPING type STRING
    returning
      value(R) type ref to ZTBOX_CL_STATS_GROUP
    raising
      resumable(ZCX_TBOX_STATS) .
  methods OUTLIERS
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type STRING_TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods HISTOGRAM
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_HIST_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods EMPIRICAL_PDF
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_VALUES_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods EMPIRICAL_CDF
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_EDIST_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods CORRELATION
    importing
      !COLS type STRING
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COVARIANCE
    importing
      !COLS type STRING
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods STANDARD
    importing
      !SIZE type I default 1
    returning
      value(R) type TY_FLOATS
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods NORMAL
    importing
      !MEAN type F default 0
      !VARIANCE type F default 1
      !SIZE type I default 1
    returning
      value(R) type TY_FLOATS
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods BINOMIAL
    importing
      !N type I default 2
      !P type F default `0.5`
      !SIZE type I default 1
    returning
      value(R) type TY_INTS
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods GEOMETRIC
    importing
      !P type F default `0.5`
      !SIZE type I default 1
    returning
      value(R) type TY_INTS
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods POISSON
    importing
      !L type F default `1.0`
      !SIZE type I default 1
    returning
      value(R) type TY_INTS
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods BERNOULLI
    importing
      !P type F default `0.5`
      !SIZE type I default 1
    returning
      value(R) type TY_INTS
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods UNIFORM
    importing
      !LOW type F default 0
      !HIGH type F default 1
      !SIZE type I default 1
    returning
      value(R) type TY_FLOATS
    raising
      resumable(ZCX_TBOX_STATS) .
  methods ARE_NORMAL
    importing
      !COL type NAME_FELD default `TABLE_LINE`
      !ALPHA type F default `0.5`
    exporting
      !P_VALUE type F
    returning
      value(R) type ABAP_BOOL
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COL
    importing
      !COL type NAME_FELD
    returning
      value(R) type ref to ZTBOX_CL_STATS
    raising
      resumable(ZCX_TBOX_STATS) .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_buffers,
        col    TYPE name_feld,
        object TYPE string,
        data   TYPE REF TO data,
      END OF ty_buffers .
  types:
    ty_buffers_t TYPE TABLE OF ty_buffers .
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
  data _BUFFERS type TY_BUFFERS_T .

  class-methods _SET_NUMERICAL_TYPES .
  methods _CREATE_BINS
    importing
      !COL type NAME_FELD
    returning
      value(R) type TY_BINS
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _SET_VALUES
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _GET_VALUES
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_STRING_VALUES .
  methods _GET_CAT_VALUES
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_STRING_VALUES .
  methods _GET_NUM_VALUES
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type TY_FLOAT_VALUES .
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
  class-methods _UNIQUE
    importing
      !TAB type TY_FLOATS
    returning
      value(R) type F .
  class-methods _CHECK_SIZE
    importing
      !S type I
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods _CHECK_SIGMA
    importing
      !S type F
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods _CHECK_LAMBDA
    importing
      !L type F
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods _CHECK_N
    importing
      !N type I
    raising
      resumable(ZCX_TBOX_STATS) .
  class-methods _CHECK_P
    importing
      !P type F
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _ADD_TO_BUFFER
    importing
      !COL type NAME_FELD default `TABLE_LINE`
      !OBJECT type STRING
      !DATA type ANY .
  methods _BUFFERIZED
    importing
      !COL type NAME_FELD default `TABLE_LINE`
      !OBJECT type STRING
    returning
      value(R) type ABAP_BOOL .
  methods _GET_FROM_BUFFER
    importing
      !COL type NAME_FELD default `TABLE_LINE`
      !OBJECT type STRING
    exporting
      !DATA type ANY .
ENDCLASS.



CLASS ZTBOX_CL_STATS IMPLEMENTATION.


  METHOD are_normal.
**********************************************************************
*   Jarque-Bera normality test
**********************************************************************

    _check_num_col( col ).

    DATA(values) = _get_num_values( col ).

    DATA(total) = CONV f( count( ) ).
    DATA(skew)  = CONV f( skewness( col ) ).
    DATA(kurt)  = CONV f( kurtosis( col ) ).

    DATA(jb) = ( ( total + `1.0` ) / `6.0` ) * ( skew ** 2 + ( ( kurt - `3.0` ) ** 2 ) / `4.0` ).

    p_value = exp( ( `-1.0` ) * jb / `2.0` ).

    r = xsdbool( p_value > alpha ).

  ENDMETHOD.


  METHOD bernoulli.

    _check_p( p ).
    _check_size( size ).

    DATA(unif) = uniform( size = size ).

    r = VALUE #( FOR u IN unif ( COND #( WHEN u LE p THEN 1 ELSE 0 ) ) ).

  ENDMETHOD.


  METHOD binomial.

    _check_p( p ).
    _check_n( n ).
    _check_size( size ).

    r = VALUE #( FOR i = 0 UNTIL i = size ( REDUCE f( LET b = bernoulli( size = n p = p ) IN INIT s = 0 FOR x IN b NEXT s = s + x ) ) ).

  ENDMETHOD.


  METHOD class_constructor.

    _set_numerical_types( ).

  ENDMETHOD.


  METHOD coefficient_variation.

    IF _bufferized( col = col object = `CV` ).
      _get_from_buffer( EXPORTING col = col object = `CV` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(dev)   = CONV f( standard_deviation( col ) ).
    DATA(mean)  = CONV f( mean( col ) ).

    CHECK mean IS NOT INITIAL.

    r = dev / mean.

    _add_to_buffer( col = col object = `CV` data = r ).

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

    CHECK std_dev_1 IS NOT INITIAL.
    CHECK std_dev_2 IS NOT INITIAL.

    r = CONV f( covariance( cols ) ) / ( std_dev_1 * std_dev_2 ).

  ENDMETHOD.


  METHOD count.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN _data->* TO <tab>.

    r = lines( <tab> ).

  ENDMETHOD.


  METHOD count_distinct.

    _check_col( col ).

    r = lines( distinct( col ) ).

  ENDMETHOD.


  METHOD count_not_initial.

    _check_col( col ).

    DATA(values) = _get_values( col ).

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

    r = REDUCE f( INIT co = CONV f( 0 ) FOR i = 1 UNTIL i = total + 1 NEXT co = co + val_1[ i ]-value * val_2[ i ]-value - mean_1 * mean_2 ) / ( total - `1.0` ).

  ENDMETHOD.


  METHOD dispersion_index.

    IF _bufferized( col = col object = `DI` ).
      _get_from_buffer( EXPORTING col = col object = `DI` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(var)   = CONV f( variance( col ) ).
    DATA(mean)  = CONV f( mean( col ) ).

    CHECK mean IS NOT INITIAL.

    r = var / mean.

    _add_to_buffer( col = col object = `DI` data = r ).

  ENDMETHOD.


  METHOD distinct.

    IF _bufferized( col = col object = `DST` ).
      _get_from_buffer( EXPORTING col = col object = `DST` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_col( col ).

    r = COND #(
      WHEN line_exists( _categorial_values[ column = col ] )
        THEN _cat_distinct( col )
      WHEN line_exists( _numerical_values[ column = col ] )
        THEN _num_distinct( col ) ).

    _add_to_buffer( col = col object = `DST` data = r ).

  ENDMETHOD.


  METHOD empirical_cdf.

    IF _bufferized( col = col object = `ECDF` ).
      _get_from_buffer( EXPORTING col = col object = `ECDF` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(total)       = CONV f( count( ) ).
    DATA(dist_values) = values.

    SORT dist_values BY value.
    DELETE ADJACENT DUPLICATES FROM dist_values COMPARING value.

    CHECK total IS NOT INITIAL.

    SORT values BY value.

    r = VALUE #( FOR dv IN dist_values
          ( x = dv-value ) ).

    FIELD-SYMBOLS <r> LIKE LINE OF r.
    DATA ix   TYPE i VALUE 0.
    DATA prev TYPE f.

    LOOP AT values INTO DATA(val).

      IF <r> IS NOT ASSIGNED OR val-value > <r>-x.
        prev = COND f( WHEN <r> IS ASSIGNED THEN <r>-y ELSE `0.0` ).
        ix = ix + 1.
        READ TABLE r ASSIGNING <r> INDEX ix.
        <r>-y = prev.
      ENDIF.

      <r>-y = <r>-y + `1.0`.

    ENDLOOP.

    LOOP AT r ASSIGNING <r>.
      <r>-y = <r>-y / total.
    ENDLOOP.

    _add_to_buffer( col = col object = `ECDF` data = r ).

  ENDMETHOD.


  METHOD empirical_pdf.

    IF _bufferized( col = col object = `EPDF` ).
      _get_from_buffer( EXPORTING col = col object = `EPDF` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(hist)  = histogram( col ).
    DATA(n)     = CONV f( count( ) ).

    CHECK n IS NOT INITIAL.

    r = VALUE #( FOR _hist IN hist ( x = _hist-x y = CONV f( _hist-y ) / n ) ).

    _add_to_buffer( col = col object = `EPDF` data = r ).

  ENDMETHOD.


  METHOD first_quartile.

    IF _bufferized( col = col object = `FQR` ).
      _get_from_buffer( EXPORTING col = col object = `FQR` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(total)     = count( ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    CHECK values IS NOT INITIAL.
    SORT values BY value.

    DATA(left_ix)   = floor( CONV f( ( total + 1 ) / 4 ) ).
    DATA(right_ix)  = ceil( CONV f( ( total + 1 ) / 4 ) ).

    r = <val> = ( values[ left_ix ]-value + values[ right_ix ]-value ) / 2.

    _add_to_buffer( col = col object = `FQR` data = r ).

  ENDMETHOD.


  METHOD geometric.

    _check_p( p ).
    _check_size( size ).

    r = VALUE #( FOR i = 0 UNTIL i = size ( ceil( log( 1 - _unique( uniform( ) ) ) / log( 1 - p ) ) ) ).

  ENDMETHOD.


  METHOD geometric_mean.

    IF _bufferized( col = col object = `GME` ).
      _get_from_buffer( EXPORTING col = col object = `GME` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(lines)   = CONV f( count( ) ).

    CHECK values IS NOT INITIAL.

    r = REDUCE #( INIT gm = CONV f( 1 ) FOR val IN values NEXT gm = gm * ( val-value ** ( `1.0` / lines ) ) ).

    _add_to_buffer( col = col object = `GME` data = r ).

  ENDMETHOD.


  METHOD group_by.

    SPLIT grouping AT `,` INTO TABLE DATA(fields_group).

    r = NEW #(
      data      = _data
      grouping  = fields_group ).

  ENDMETHOD.


  METHOD harmonic_mean.

    IF _bufferized( col = col object = `HME` ).
      _get_from_buffer( EXPORTING col = col object = `HME` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(lines)   = CONV f( count( ) ).

    CHECK values IS NOT INITIAL.
    IF line_exists( values[ value = `0.0` ] ).
      r = `0`.
      RETURN.
    ENDIF.

    r = lines / REDUCE #( INIT sr = CONV f( 0 ) FOR val IN values NEXT sr = sr + ( `1.0` / val-value ) ).

    _add_to_buffer( col = col object = `HME` data = r ).

  ENDMETHOD.


  METHOD histogram.

    IF _bufferized( col = col object = `HIST` ).
      _get_from_buffer( EXPORTING col = col object = `HIST` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(bins)    = _create_bins( col ).

    r = VALUE #( FOR _bin IN bins
      ( x = _bin-x ) ).

    SORT values BY value.

    FIELD-SYMBOLS <r> LIKE LINE OF r.
    DATA bin  LIKE LINE OF bins.
    DATA ix   TYPE i VALUE 0.

    LOOP AT values INTO DATA(val).

      IF <r> IS NOT ASSIGNED OR val-value NOT BETWEEN bin-x AND bin-y.
        ix = ix + 1.
        READ TABLE bins INTO bin INDEX ix.
        READ TABLE r ASSIGNING <r> WITH KEY x = bin-x.
      ENDIF.

      <r>-y = <r>-y + 1.

    ENDLOOP.

    _add_to_buffer( col = col object = `HIST` data = r ).

  ENDMETHOD.


  METHOD interquartile_range.

    IF _bufferized( col = col object = `IQR` ).
      _get_from_buffer( EXPORTING col = col object = `IQR` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    r = <val> = third_quartile( col ) - first_quartile( col ).

    _add_to_buffer( col = col object = `IQR` data = r ).

  ENDMETHOD.


  METHOD kurtosis.

    IF _bufferized( col = col object = `KRT` ).
      _get_from_buffer( EXPORTING col = col object = `KRT` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(std_dev)     = CONV f( standard_deviation( col ) ).
    DATA(mean_value)  = CONV f( mean( col ) ).
    DATA(total)       = CONV f( count( ) ).

    CHECK total > `1.0`.
    CHECK std_dev IS NOT INITIAL.

    r = REDUCE #( INIT v = CONV f( 0 ) FOR val IN values NEXT v = v + ( ( val-value - mean_value ) / std_dev ) ** 4 ) / ( total - `1.0` ).

    _add_to_buffer( col = col object = `KRT` data = r ).

  ENDMETHOD.


  METHOD mad_mean.

    IF _bufferized( col = col object = `MAD_MEA` ).
      _get_from_buffer( EXPORTING col = col object = `MAD_MEA` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(total)   = CONV f( count( ) ).
    DATA(values)  = _get_num_values( col ).
    DATA(mean)    = CONV f( mean( col ) ).

    CHECK total IS NOT INITIAL.

    r = REDUCE f( INIT mad = CONV f( 0 ) FOR v IN values NEXT mad = mad + abs( v-value - mean ) ) / total.

    _add_to_buffer( col = col object = `MAD_MEA` data = r ).

  ENDMETHOD.


  METHOD mad_median.

    IF _bufferized( col = col object = `MAD_MED` ).
      _get_from_buffer( EXPORTING col = col object = `MAD_MED` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(total)   = CONV f( count( ) ).
    DATA(values)  = _get_num_values( col ).
    DATA(median)  = CONV f( median( col ) ).

    CHECK total IS NOT INITIAL.

    r = REDUCE f( INIT mad = CONV f( 0 ) FOR v IN values NEXT mad = mad + abs( v-value - median ) ) / total.

    _add_to_buffer( col = col object = `MAD_MED` data = r ).

  ENDMETHOD.


  METHOD max.

    IF _bufferized( col = col object = `MAX` ).
      _get_from_buffer( EXPORTING col = col object = `MAX` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    CHECK values IS NOT INITIAL.

    SORT values BY value DESCENDING.
    r = <val> = values[ 1 ]-value.

    _add_to_buffer( col = col object = `MAX` data = r ).

  ENDMETHOD.


  METHOD mean.

    IF _bufferized( col = col object = `MEA` ).
      _get_from_buffer( EXPORTING col = col object = `MEA` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    CHECK count( ) IS NOT INITIAL.

    r = CONV f( sum( col ) ) / CONV f( count( ) ).

    _add_to_buffer( col = col object = `MEA` data = r ).

  ENDMETHOD.


  METHOD median.

    _check_num_col( col ).

    r = second_quartile( col ).

  ENDMETHOD.


  METHOD min.

    IF _bufferized( col = col object = `MIN` ).
      _get_from_buffer( EXPORTING col = col object = `MIN` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    CHECK values IS NOT INITIAL.

    SORT values BY value.
    r = <val> = values[ 1 ]-value.

    _add_to_buffer( col = col object = `MIN` data = r ).

  ENDMETHOD.


  METHOD normal.

    _check_sigma( variance ).
    _check_size( size ).

    DATA(s) = standard( size ).

    r = VALUE #( FOR x IN s ( x * sqrt( variance ) + mean ) ).

  ENDMETHOD.


  METHOD normalize.

    _check_num_col( col ).

    DATA(min) = CONV f( min( col ) ).
    DATA(max) = CONV f( max( col ) ).

    CHECK min NE max.

    READ TABLE _numerical_values ASSIGNING FIELD-SYMBOL(<cat>) WITH KEY column = col.

    LOOP AT <cat>-values ASSIGNING FIELD-SYMBOL(<val>).
      <val>-value = ( <val>-value - min ) / ( max - min ).
    ENDLOOP.

    DELETE _buffers WHERE col = col.

    r = VALUE #( FOR v IN <cat>-values ( v-value ) ).

  ENDMETHOD.


  METHOD outliers.

    IF _bufferized( col = col object = `OUT` ).
      _get_from_buffer( EXPORTING col = col object = `OUT` IMPORTING data = r ).
      RETURN.
    ENDIF.

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

    _add_to_buffer( col = col object = `OUT` data = r ).

  ENDMETHOD.


  METHOD poisson.

    r = VALUE #( FOR i = 0 UNTIL i = size ( REDUCE #( INIT k = 0 FOR p = CONV f( 1 ) THEN p * _unique( uniform( ) ) WHILE p > exp( ( -1 ) * l ) NEXT k = k + 1 ) - 1 ) ).

  ENDMETHOD.


  METHOD quadratic_mean.

    IF _bufferized( col = col object = `QME` ).
      _get_from_buffer( EXPORTING col = col object = `QME` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)  = _get_num_values( col ).
    DATA(total)   = CONV f( count( ) ).

    CHECK values IS NOT INITIAL.

    r = sqrt( REDUCE #( INIT sr = CONV f( 0 ) FOR val IN values NEXT sr = sr + ( val-value ) ** 2 ) / total ).

    _add_to_buffer( col = col object = `QME` data = r ).

  ENDMETHOD.


  METHOD range.

    IF _bufferized( col = col object = `RAN` ).
      _get_from_buffer( EXPORTING col = col object = `RAN` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    r = <val> = max( ) - min( ).

    _add_to_buffer( col = col object = `RAN` data = r ).

  ENDMETHOD.


  METHOD second_quartile.

    IF _bufferized( col = col object = `SQR` ).
      _get_from_buffer( EXPORTING col = col object = `SQR` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(total)     = count( ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    CHECK values IS NOT INITIAL.
    SORT values BY value.

    DATA(left_ix)   = floor( CONV f( ( total + 1 ) / 2 ) ).
    DATA(right_ix)  = ceil( CONV f( ( total + 1 ) / 2 ) ).

    r = <val> = ( values[ left_ix ]-value + values[ right_ix ]-value ) / 2.

    _add_to_buffer( col = col object = `SQR` data = r ).

  ENDMETHOD.


  METHOD skewness.

    IF _bufferized( col = col object = `SKW` ).
      _get_from_buffer( EXPORTING col = col object = `SKW` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(std_dev)     = CONV f( standard_deviation( col ) ).
    DATA(mean_value)  = CONV f( mean( col ) ).
    DATA(total)       = CONV f( count( ) ).

    CHECK total > `2.0`.
    CHECK std_dev IS NOT INITIAL.

    r = total * REDUCE #( INIT v = CONV f( 0 ) FOR val IN values NEXT v = v + ( ( val-value - mean_value ) / std_dev ) ** 3 ) / ( ( total - `1.0` ) * ( total - `2.0` ) ).

    _add_to_buffer( col = col object = `SKW` data = r ).

  ENDMETHOD.


  METHOD standard.
**********************************************************************
*   Box–Muller transform sampling method
**********************************************************************

    _check_size( size ).

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

    CHECK std_dev IS NOT INITIAL.

    READ TABLE _numerical_values ASSIGNING FIELD-SYMBOL(<cat>) WITH KEY column = col.

    LOOP AT <cat>-values ASSIGNING FIELD-SYMBOL(<val>).
      <val>-value = ( <val>-value - mean ) / std_dev.
    ENDLOOP.

    DELETE _buffers WHERE col = col.

    r = VALUE #( FOR v IN <cat>-values ( v-value ) ).

  ENDMETHOD.


  METHOD standard_deviation.

    IF _bufferized( col = col object = `DEV` ).
      _get_from_buffer( EXPORTING col = col object = `DEV` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    r = sqrt( CONV f( variance( col ) ) ).

    _add_to_buffer( col = col object = `DEV` data = r ).


  ENDMETHOD.


  METHOD sum.

    IF _bufferized( col = col object = `SUM` ).
      _get_from_buffer( EXPORTING col = col object = `SUM` IMPORTING data = r ).
      RETURN.
    ENDIF.

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

    _add_to_buffer( col = col object = `SUM` data = r ).

  ENDMETHOD.


  METHOD third_quartile.

    IF _bufferized( col = col object = `TQR` ).
      _get_from_buffer( EXPORTING col = col object = `TQR` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)    = _get_num_values( col ).
    DATA(total)     = count( ).
    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    CHECK values IS NOT INITIAL.
    SORT values BY value.

    DATA(left_ix)   = floor( CONV f( ( 3 * ( total + 1 ) ) / 4 ) ).
    DATA(right_ix)  = ceil( CONV f( ( 3 * ( total + 1 ) ) / 4 ) ).

    r = <val> = ( values[ left_ix ]-value + values[ right_ix ]-value ) / 2.

    _add_to_buffer( col = col object = `TQR` data = r ).

  ENDMETHOD.


  METHOD uniform.

    _check_size( size ).

    DATA(randomizer) = cl_abap_random_float=>create( cl_abap_random=>seed( ) ).

    DO size TIMES.

      DATA(rand_float) = randomizer->get_next( ).

      APPEND ( low + ( high - low ) * rand_float ) TO r.

    ENDDO.

  ENDMETHOD.


  METHOD variance.

    IF _bufferized( col = col object = `VAR` ).
      _get_from_buffer( EXPORTING col = col object = `VAR` IMPORTING data = r ).
      RETURN.
    ENDIF.

    _check_num_col( col ).

    DATA(values)      = _get_num_values( col ).
    DATA(mean_value)  = CONV f( mean( col ) ).
    DATA(total)       = CONV f( count( ) ).

    CHECK total > `1.0`.

    r = REDUCE #( INIT v = CONV f( 0 ) FOR val IN values NEXT v = v + ( val-value - mean_value ) ** 2 ) / ( total - `1.0` ).

    _add_to_buffer( col = col object = `VAR` data = r ).

  ENDMETHOD.


  METHOD _add_to_buffer.

    DATA data_ref TYPE REF TO data.

    CREATE DATA data_ref LIKE data.
    ASSIGN data_ref->* TO FIELD-SYMBOL(<data>).
    <data> = data.

    APPEND VALUE #(
      col     = col
      object  = object
      data    = data_ref ) TO _buffers.

  ENDMETHOD.


  METHOD _bufferized.

    r = xsdbool( line_exists( _buffers[ col = col object = object ] ) ).

  ENDMETHOD.


  METHOD _cat_distinct.

    DATA(values) = _get_cat_values( col ).

    SORT values BY value.
    DELETE ADJACENT DUPLICATES FROM values COMPARING value.

    r = VALUE #( FOR v IN values ( v-value ) ).

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


  METHOD _check_lambda.

    IF NOT l > `0.0`.
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>l_not_valid.
    ENDIF.

  ENDMETHOD.


  METHOD _check_n.

    IF NOT n > 0.
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>n_not_valid.
    ENDIF.

  ENDMETHOD.


  METHOD _check_num_col.

    IF NOT line_exists( _numerical_values[ column = col ] ).
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>column_not_numerical.
    ENDIF.

  ENDMETHOD.


  METHOD _check_p.

    IF p NOT BETWEEN `0.0` AND `1.0`.
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>p_not_valid.
    ENDIF.

  ENDMETHOD.


  METHOD _check_sigma.

    IF NOT s > `0.0`.
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>v_not_valid.
    ENDIF.

  ENDMETHOD.


  METHOD _check_size.

    IF NOT s > 0.
      RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>s_not_valid.
    ENDIF.

  ENDMETHOD.


  METHOD _create_bins.

    DATA(total)   = CONV f( count( ) ).
    DATA(min)     = CONV f( min( ) ).
    DATA(max)     = CONV f( max( ) ).
    DATA(iqr)     = CONV f( interquartile_range( ) ).

**********************************************************************
*   Freedman-Diaconis rule
**********************************************************************
    DATA(bin_width) = ( iqr / ( total ** ( 1 / 3 ) ) ) * 2.
    DATA(bin_total) = CONV i( ( max - min ) / bin_width ).

    r = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = bin_total ( x = min + i * bin_width y = min + ( i + 1 ) * bin_width ) ).

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


  METHOD _get_cat_values.

    r = _categorial_values[ column = col ]-values.

  ENDMETHOD.


  METHOD _get_from_buffer.

    DATA(data_ref) = _buffers[ col = col object = object ]-data.

    ASSIGN data_ref->* TO FIELD-SYMBOL(<data>).
    data = <data>.

  ENDMETHOD.


  METHOD _get_num_values.

    r = _numerical_values[ column = col ]-values.

  ENDMETHOD.


  METHOD _get_values.

    IF line_exists( _categorial_values[ column = col ] ).
      r = _categorial_values[ column = col ]-values.
      RETURN.
    ENDIF.

    IF line_exists( _numerical_values[ column = col ] ).
      r = VALUE #( FOR n IN _numerical_values[ column = col ]-values ( ix = n-ix value = CONV string( n-value ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD _num_distinct.

    DATA(values) = _get_num_values( col ).

    SORT values BY value.
    DELETE ADJACENT DUPLICATES FROM values COMPARING value.

    DATA(value_ref) = _create_data_like( col ).
    ASSIGN value_ref->* TO FIELD-SYMBOL(<val>).

    LOOP AT values INTO DATA(val).

      <val> = val-value.
      APPEND <val> TO r.

    ENDLOOP.

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


  METHOD _unique.

    r = tab[ 1 ].

  ENDMETHOD.
ENDCLASS.
