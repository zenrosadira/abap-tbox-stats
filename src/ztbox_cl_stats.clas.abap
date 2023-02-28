class ZTBOX_CL_STATS definition
  public
  final
  create public .

public section.

  types:
    ty_p      TYPE p LENGTH 16 DECIMALS 4 .
  types:
    ty_p_int  TYPE TABLE OF ty_p WITH DEFAULT KEY .
  types:
    BEGIN OF ty_p_edf,
        x TYPE ty_p,
        y TYPE ty_p,
      END OF ty_p_edf .
  types:
    ty_p_edf_t TYPE TABLE OF ty_p_edf WITH DEFAULT KEY .
  types:
    BEGIN OF ty_p_pmf,
        x TYPE ty_p,
        y TYPE i,
      END OF ty_p_pmf .
  types:
    ty_p_pmf_t TYPE TABLE OF ty_p_pmf WITH DEFAULT KEY .
  types:
    BEGIN OF ty_p_freq,
        data TYPE ty_p,
        freq TYPE i,
      END OF ty_p_freq .
  types:
    ty_p_freq_t TYPE TABLE OF ty_p_freq WITH DEFAULT KEY .

  class-methods GET_INTERVAL
    importing
      !I_MIN type TY_P default 0
      !I_MAX type TY_P default 1
      !I_STEP type TY_P
    returning
      value(R_INT) type TY_P_INT .
  methods SET_DATA
    importing
      !I_DATA type TY_P_INT .
  methods GET_DATA
    returning
      value(R_OUT) type TY_P_INT .
  methods MEAN
    returning
      value(R_RES) type TY_P .
  methods COUNT
    returning
      value(R_RES) type TY_P .
  methods SUM
    returning
      value(R_RES) type TY_P .
  methods MAX
    returning
      value(R_RES) type TY_P .
  methods MODE
    returning
      value(R_RES) type TY_P
    raising
      ZCX_TBOX_STATS .
  methods QUARTILE
    importing
      !I_QUARTILE type I
    returning
      value(R_RES) type TY_P .
  methods IQR
    returning
      value(R_RES) type TY_P .
  methods MIN
    returning
      value(R_RES) type TY_P .
  methods FREQUENCIES
    returning
      value(R_FREQ) type TY_P_FREQ_T .
  methods DEV_STD
    returning
      value(R_RES) type TY_P .
  methods VARIANCE
    returning
      value(R_RES) type TY_P .
  methods EPMF
    returning
      value(R_EDF) type TY_P_PMF_T .
  methods ECDF
    returning
      value(R_EDF) type TY_P_EDF_T .
  methods OUTLIERS
    returning
      value(R_OUT) type TY_P_INT .
  methods REMOVE_OUTLIERS .
  methods FEATURE_SCALING .
  methods STANDARD_NORMALIZATION .
protected section.
private section.

  data _ORIGINAL_DATA type TY_P_INT .
  data _WORK_DATA type TY_P_INT .

  methods _DIST_QUAD
    importing
      !X type TY_P
      !Y type TY_P
    returning
      value(R) type TY_P .
ENDCLASS.



CLASS ZTBOX_CL_STATS IMPLEMENTATION.


  METHOD count.

    r_res = lines( _work_data ).

  ENDMETHOD.


  METHOD dev_std.

    r_res = sqrt( variance( ) ).

  ENDMETHOD.


  METHOD ECDF.

    DATA count TYPE i.

    DATA(x_line) = get_interval(
      i_min   = min( )
      i_max   = max( )
      i_step  = dev_std( ) / 100 ).

    LOOP AT x_line INTO DATA(x).

      CLEAR count.
      LOOP AT _work_data INTO DATA(data) WHERE table_line LE x.
        ADD 1 TO count.
      ENDLOOP.

      APPEND VALUE #(
        x = x
        y = count / lines( _work_data ) ) TO r_edf.

    ENDLOOP.

  ENDMETHOD.


  METHOD epmf.

    DATA count    TYPE i.
    DATA ix_prev  TYPE i.

    DATA(x_line) = get_interval(
      i_min   = min( )
      i_max   = max( )
      i_step  = dev_std( ) / 100 ).

    LOOP AT x_line INTO DATA(x) FROM 2.
      ix_prev = sy-tabix - 1.

      DATA(x_prev) = VALUE #( x_line[ ix_prev ] OPTIONAL ).

      CLEAR count.
      LOOP AT _work_data INTO DATA(data) WHERE table_line GE x_prev AND table_line LE x.
        ADD 1 TO count.
      ENDLOOP.

      APPEND VALUE #(
        x = x
        y = count ) TO r_edf.

    ENDLOOP.

  ENDMETHOD.


  METHOD feature_scaling.

    DATA(min) = min( ).
    DATA(max) = max( ).

    LOOP AT _work_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      <fs_data> = ( <fs_data> - min ) / ( max - min ).

    ENDLOOP.

  ENDMETHOD.


  METHOD frequencies.

    LOOP AT _work_data INTO DATA(data).

      READ TABLE r_freq ASSIGNING FIELD-SYMBOL(<fs_freq>) WITH KEY data = data.

      CASE sy-subrc.

        WHEN 0.

          <fs_freq>-freq += 1.

        WHEN OTHERS.

          APPEND VALUE #(
                  data = data
                  freq = 1 ) TO r_freq.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_data.

    r_out = _work_data.

  ENDMETHOD.


  METHOD get_interval.

    DATA current TYPE ty_p.

    current = i_min.

    WHILE current LE i_max.

      APPEND current TO r_int.

      current += i_step.

    ENDWHILE.

  ENDMETHOD.


  METHOD iqr.

    r_res = quartile( 3 ) - quartile( 1 ).

  ENDMETHOD.


  METHOD max.

    CHECK _work_data IS NOT INITIAL.

    r_res = _work_data[ lines( _work_data ) ].

  ENDMETHOD.


  METHOD mean.

    CHECK count( ) IS NOT INITIAL.

    r_res = sum( ) / count( ).

  ENDMETHOD.


  METHOD min.

    CHECK line_exists( _work_data[ 1 ] ).

    r_res = _work_data[ 1 ].

  ENDMETHOD.


  METHOD mode.

    DATA(frequencies) = frequencies( ).

    SORT frequencies BY freq DESCENDING.

    r_res = VALUE #( frequencies[ 1 ]-data OPTIONAL ).

    DATA(second) = VALUE #( frequencies[ 2 ]-data OPTIONAL ).

    IF second IS NOT INITIAL AND second EQ r_res.
      RAISE EXCEPTION NEW zcx_tbox_stats( error = 'NO MODE' ).
    ENDIF.

  ENDMETHOD.


  METHOD outliers.

    DATA(q1)    = quartile( 1 ).
    DATA(q3)    = quartile( 3 ).
    DATA(iqr)   = q3 - q1.
    DATA(low)   = q1 - '1.5' * iqr.
    DATA(high)  = q3 + '1.5' * iqr.

    r_out = VALUE #( FOR _data IN _work_data WHERE ( table_line NOT BETWEEN low AND high ) ( _data ) ).

  ENDMETHOD.


  METHOD quartile.

    DATA(threshold) = COND ty_p(
      WHEN i_quartile EQ 1 THEN '0.25'
      WHEN i_quartile EQ 2 THEN '0.50'
      WHEN i_quartile EQ 3 THEN '0.75'
        ELSE 0 ).

    CHECK threshold IS NOT INITIAL.

    DATA(ecdf) = ecdf( ).

    SORT ecdf BY y ASCENDING.

    LOOP AT ecdf INTO DATA(p).

      IF p-y > threshold.
        EXIT.
      ENDIF.

      r_res = p-x.

    ENDLOOP.

  ENDMETHOD.


  METHOD remove_outliers.

    DATA(outliers) = outliers( ).

    LOOP AT _work_data INTO DATA(data).

      CHECK line_exists( outliers[ table_line = data ] ).

      DELETE _work_data INDEX sy-tabix.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_data.

    _original_data  = i_data.
    _work_data      = i_data.

    SORT _work_data ASCENDING.

  ENDMETHOD.


  METHOD standard_normalization.

    DATA(mean)    = mean( ).
    DATA(dev_std) = dev_std( ).

    LOOP AT _work_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      <fs_data> = ( <fs_data> - mean ) / dev_std.

    ENDLOOP.

  ENDMETHOD.


  METHOD sum.

    r_res = REDUCE #( INIT s = 0 FOR _data IN _work_data NEXT s += _data ).

  ENDMETHOD.


  METHOD variance.

    LOOP AT _work_data INTO DATA(data).

      TRY.
          DATA(dist_q) = _dist_quad( x = data y = mean( ) ).
        CATCH cx_sy_arithmetic_overflow.
      ENDTRY.

      r_res += dist_q.

    ENDLOOP.

    r_res = r_res / lines( _work_data ).

  ENDMETHOD.


  METHOD _DIST_QUAD.

    r = ( x - y ) ** 2.

  ENDMETHOD.
ENDCLASS.
