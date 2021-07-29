CLASS zclbc_download_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_objky,
        objky TYPE nast-objky,
      END OF ty_objky,
      tab_objky TYPE TABLE OF ty_objky,

      BEGIN OF ty_nast,
        aplicacao        TYPE zcds_nast-aplicacao,
        chaveobjeto      TYPE zcds_nast-chaveobjeto,
        tipomensagem     TYPE zcds_nast-tipomensagem,
        idioma           TYPE zcds_nast-idioma,
        partner          TYPE zcds_nast-partner,
        funcao           TYPE zcds_nast-funcao,
        datacriacao      TYPE zcds_nast-datacriacao,
        horacriacao      TYPE zcds_nast-horacriacao,
        numarquivo       TYPE zcds_nast-numarquivo,
        objecttype       TYPE zcds_nast-objecttype,
        iddocumento      TYPE zcds_nast-iddocumento,
        tipodocumento    TYPE zcds_nast-tipodocumento,
        dispositivoSaida TYPE zcds_nast-dispositivoSaida,
      END OF ty_nast,
      tab_nast TYPE TABLE OF ty_nast.

    METHODS constructor
      IMPORTING
        !it_objky TYPE tab_objky OPTIONAL .

    METHODS process
      RETURNING
        VALUE(rt_value) TYPE zbc_tt_0008 .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      gt_nast TYPE zclbc_download_doc=>tab_nast .

    METHODS get_archive_param
      IMPORTING
        iv_nast            TYPE zclbc_download_doc=>ty_nast
      EXPORTING
        VALUE(iv_objectid) TYPE saeobjid
        VALUE(iv_arobject) TYPE saeobjart
        VALUE(iv_reserve)  TYPE saereserve .

    METHODS get_archive_attachment_all
      RETURNING
        VALUE(rt_value) TYPE zbc_tt_0008 .

    METHODS get_spool
      RETURNING
        VALUE(rt_value) TYPE zbc_tt_0008 .

    METHODS get_archive_attachment
      IMPORTING
        VALUE(iv_objectid) TYPE saeobjid
        VALUE(iv_arobject) TYPE saeobjart
        VALUE(iv_reserve)  TYPE saereserve
      RETURNING
        VALUE(rt_value)    TYPE xstring_table .

    METHODS get_gos_attachment_all
      RETURNING
        VALUE(rt_value) TYPE zbc_tt_0008 .

    METHODS get_gos_attachment
      IMPORTING
        VALUE(im_object_instid) TYPE sibfboriid
        VALUE(im_object_typeid) TYPE saeanwdid
        VALUE(im_object_catid)  TYPE sibfcatid
        VALUE(im_role)          TYPE oblroltype
      RETURNING
        VALUE(re_xstring_table) TYPE xstring_table .

    METHODS get_nast
      IMPORTING
        !it_objky TYPE tab_objky
      EXPORTING
        !et_nast  TYPE tab_nast .


ENDCLASS.



CLASS zclbc_download_doc IMPLEMENTATION.

  METHOD constructor .

    CLEAR:
      me->gt_nast .

    IF ( lines( it_objky ) GT 0 ) .

      me->get_nast( EXPORTING it_objky = it_objky
                    IMPORTING et_nast  = me->gt_nast ) .

    ENDIF .

  ENDMETHOD .


  METHOD process .

    CLEAR rt_value .

    " Search for the image on the ArchiveLink
    DATA(lt_data_file) = me->get_archive_attachment_all( ) .

    IF ( lines( lt_data_file ) EQ 0 ) .

      " Generate spool/file
      lt_data_file = me->get_spool( ) .

      IF ( lines( lt_data_file ) EQ 0 ) .
        " Otherwise search for the attachments linked to the Business Object
        lt_data_file = me->get_gos_attachment_all( ) .

      ENDIF.

    ENDIF.

    IF ( lines( lt_data_file ) GT 0 ) .
      APPEND LINES OF lt_data_file TO rt_value .
    ENDIF .

  ENDMETHOD .


  METHOD get_archive_param .

    CONSTANTS:
      lc_char20     TYPE char20 VALUE '....................',
      lc_extensioon TYPE char3 VALUE 'PDF'.

    DATA:
      lv_numarquivo    TYPE char10.

    CLEAR:
      iv_objectid, iv_arobject, iv_reserve .

    WRITE iv_nast-numarquivo TO lv_numarquivo .

    CONCATENATE iv_nast-chaveobjeto
                lc_char20
                lv_numarquivo
           INTO iv_objectid .

    TRANSLATE iv_objectid USING '. ' .

    iv_arobject = iv_nast-tipodocumento .
    iv_reserve = 'PDF' .

  ENDMETHOD .


  METHOD get_archive_attachment_all .

    DATA:
      lt_table_file TYPE xstring_table .

    CLEAR rt_value .

    LOOP AT me->gt_nast INTO DATA(ls_nast)  .

      me->get_archive_param( EXPORTING iv_nast         = ls_nast
                             IMPORTING iv_objectid = DATA(lv_objectid)
                                       iv_arobject = DATA(lv_ar_object)
                                       iv_reserve = DATA(lv_reserve) ) .

      lt_table_file =
        me->get_archive_attachment( iv_objectid  = lv_objectid
                                    iv_arobject  = lv_ar_object
                                    iv_reserve  = lv_reserve ) .

      IF ( lines( lt_table_file ) GT 0 ) .

        APPEND VALUE #( name = |{ ls_nast-iddocumento }.{ lv_reserve }|
                        data = lt_table_file[] ) TO rt_value .

        CLEAR:
          lt_table_file, lv_objectid, lv_ar_object, lv_reserve .

      ENDIF .

    ENDLOOP .

  ENDMETHOD .


  METHOD get_spool .

    CONSTANTS:
      lc_format TYPE char3 VALUE 'PDF' .

    DATA:
      lv_bin_file        TYPE xstring,
      ls_job_output_info TYPE ssfcrescl,
      lt_lines           TYPE esy_tt_tline.

    CLEAR rt_value.

    IF ( lines( me->gt_nast ) GT 0 ) .

      " Buscando dados de config e de saida de mensagem
      SELECT kschl, nacha, kappl, pgnam, ronam, sform
       UP TO 1 ROWS
        FROM tnapr
        INTO @DATA(ls_conf_mensagens)
         FOR ALL ENTRIES IN @me->gt_nast
       WHERE kschl EQ @me->gt_nast-tipomensagem .
      ENDSELECT .

      IF ( sy-subrc EQ 0 ) .

        DATA(ls_tnapr) =
          VALUE tnapr( kschl = ls_conf_mensagens-kschl
                       nacha = ls_conf_mensagens-nacha
                       kappl = ls_conf_mensagens-kappl
                       pgnam = ls_conf_mensagens-pgnam
                       ronam = ls_conf_mensagens-ronam
                       sform = ls_conf_mensagens-sform ) .

        DATA(ls_nast) =
          VALUE nast( kappl = ls_conf_mensagens-kappl
                      objky = VALUE #( me->gt_nast[ 1 ]-chaveobjeto OPTIONAL )
                      kschl = VALUE #( me->gt_nast[ 1 ]-tipomensagem OPTIONAL )
                      spras = sy-langu
                      erdat = sy-datum
                      eruhr = sy-uzeit
                      nacha = 1
                      anzal = 1
                      vsztp = 1
                      ldest = VALUE #( me->gt_nast[ 1 ]-dispositivosaida OPTIONAL )
                      dimme = abap_on
                      nauto = abap_on ) .

        TRY .
            PERFORM fill_form IN PROGRAM saplzbc_fg_download_doc USING ls_nast ls_tnapr .
          CATCH cx_sy_dyn_call_illegal_type .
        ENDTRY .

        " Montando variavel de retorno para ponteiro
        DATA(lv_job_output_info) = |({ ls_conf_mensagens-pgnam })E_SSFCRESCL| .

        " Captura dados exportados pelo SmartForms
        ASSIGN (lv_job_output_info) TO FIELD-SYMBOL(<fs_job_output_info>) .

        IF ( <fs_job_output_info> IS ASSIGNED ) .

          ls_job_output_info = <fs_job_output_info>.

          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              format                = lc_format
*             max_linewidth         = 132
*             archive_index         = space
*             copynumber            = 0
*             ascii_bidi_vis2log    = space
*             pdf_delete_otftab     = space
*             pdf_username          = space
*             pdf_preview           = space
*             use_cascading         = space
*             modified_param_table  =
            IMPORTING
*             bin_filesize          =
              bin_file              = lv_bin_file
            TABLES
              otf                   = ls_job_output_info-otfdata
              lines                 = lt_lines
            EXCEPTIONS
              err_max_linewidth     = 1
              err_format            = 2
              err_conv_not_possible = 3
              err_bad_otf           = 4
              OTHERS                = 5.

          IF ( sy-subrc EQ 0 ) .
            APPEND VALUE #( name = |{ sy-datum }.{ lc_format }|
                            data = VALUE #( ( lv_bin_file ) ) ) TO rt_value .
          ENDIF .

          UNASSIGN <fs_job_output_info> .

        ENDIF .

      ENDIF .

    ENDIF .


  ENDMETHOD .


  METHOD get_archive_attachment.

    "variáveis locais
    DATA:
      lt_conninfo_tab TYPE STANDARD TABLE OF toav0,
      lt_archobje_tab TYPE STANDARD TABLE OF docs,
      lt_binarcob_tab TYPE STANDARD TABLE OF tbl1024,
      lv_bin_leng_var TYPE num12,
      lv_xstrings_var TYPE xstring.

    "limpar variáveis de retorno
    REFRESH rt_value.

    "checking wheter the material image is archived on the link tables (TOA01, TOA02, TOA03)
    "get material object id
    SELECT *
      FROM toa01
      INTO TABLE @DATA(lt_arcdocid_tab)
      UP TO 1 ROWS
      WHERE object_id EQ @iv_objectid
        AND ar_object EQ @iv_arobject
        AND reserve   EQ @iv_reserve.
    IF sy-subrc IS NOT INITIAL.
      SELECT *
        FROM toa02
        INTO TABLE lt_arcdocid_tab
        UP TO 1 ROWS
        WHERE object_id EQ iv_objectid
          AND ar_object EQ iv_arobject
          AND reserve   EQ iv_reserve.
      IF sy-subrc IS NOT INITIAL.
        SELECT *
          FROM toa03
          INTO TABLE lt_arcdocid_tab
          UP TO 1 ROWS
          WHERE object_id EQ iv_objectid
            AND ar_object EQ iv_arobject
            AND reserve   EQ iv_reserve.
      ENDIF.
    ENDIF.

    DATA(ls_arcdocid_str) = COND #( WHEN line_exists( lt_arcdocid_tab[ 1 ] )
                                    THEN lt_arcdocid_tab[ 1 ] ).
    IF ls_arcdocid_str IS INITIAL.
      "sair do processamento
      RETURN.
    ENDIF.

    "recover image from archiveLink
    CALL FUNCTION 'ARCHIV_CONNECTINFO_GET_META'
      EXPORTING
        ar_object             = ls_arcdocid_str-ar_object
        object_id             = ls_arcdocid_str-object_id
        sap_object            = ls_arcdocid_str-sap_object
      TABLES
        connect_info          = lt_conninfo_tab
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.
    IF sy-subrc IS NOT INITIAL.
      "sair do processamento
      RETURN.
    ENDIF.

    "get only the more recent image
    SORT lt_conninfo_tab DESCENDING BY ar_date.
    DATA(ls_conninfo_str) = lt_conninfo_tab[ 1 ].
    REFRESH: lt_conninfo_tab.
    APPEND ls_conninfo_str TO lt_conninfo_tab.

    "link table
    LOOP AT lt_conninfo_tab ASSIGNING FIELD-SYMBOL(<fs_conninfo_str>)
      WHERE reserve EQ iv_reserve.

      "bynary format Object
      CLEAR:
        lv_bin_leng_var, lv_xstrings_var.
      REFRESH:
        lt_archobje_tab, lt_binarcob_tab.

      CALL FUNCTION 'ARCHIVOBJECT_GET_BYTES'
        EXPORTING
          archiv_id                = <fs_conninfo_str>-archiv_id
          archiv_doc_id            = <fs_conninfo_str>-arc_doc_id
          document_type            = CONV saedoktyp( iv_reserve )
          length                   = 0
          offset                   = 0
        IMPORTING
          length                   = lv_bin_leng_var
        TABLES
          archivobject             = lt_archobje_tab
          binarchivobject          = lt_binarcob_tab
        EXCEPTIONS
          error_archiv             = 1
          error_communicationtable = 2
          error_kernel             = 3
          OTHERS                   = 4.
      IF sy-subrc IS NOT INITIAL.
        " Passar para o próximo registo
        CONTINUE.
      ENDIF.

      " Convert Binary table to Xstring
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = CONV i( lv_bin_leng_var )
        IMPORTING
          buffer       = lv_xstrings_var
        TABLES
          binary_tab   = lt_binarcob_tab
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc IS NOT INITIAL.
        "passar para o próximo registo
        CONTINUE.
      ENDIF.

      APPEND lv_xstrings_var TO rt_value .
      CLEAR  lv_xstrings_var .

    ENDLOOP.

  ENDMETHOD.


  METHOD get_gos_attachment_all .


    CONSTANTS:
      lc_char20 TYPE char20 VALUE '....................' .

    DATA:
      lt_xstring_table TYPE xstring_table,
      lv_objectid      TYPE toa01-object_id,
      lv_ar_object     TYPE toa01-ar_object,
      lv_reserve       TYPE toa01-reserve,
      lv_numarquivo    TYPE char10.

    CLEAR rt_value .

    LOOP AT me->gt_nast INTO DATA(ls_nast)  .

      WRITE ls_nast-numarquivo TO lv_numarquivo .

      CONCATENATE ls_nast-chaveobjeto
                  lc_char20
                  lv_numarquivo
             INTO lv_objectid .

      TRANSLATE lv_objectid USING '. ' .

      lv_ar_object = ls_nast-tipodocumento .
      lv_reserve   = 'PDF' .

*      " OtherWise serach for the attachments linked to the Business Object
*      lt_xstring_table =
*        me->get_gos_attachment( im_object_instid = CONV #( lv_object ) " Material Number
*                                im_object_typeid = 'BUS1001006'       " Business object - Standard Material
*                                im_object_catid  = 'BO'               " Business Object
*                                im_role          = 'GOSAPPLOBJ'       " Generic object services application object
*                                                            ).

      IF ( lines( lt_xstring_table ) GT 0 ) .

        APPEND VALUE #( name = |{ ls_nast-iddocumento }.{ lv_reserve }|
                        data = lt_xstring_table[] ) TO rt_value .

        CLEAR:
          lt_xstring_table, lv_objectid, lv_ar_object, lv_reserve .

      ENDIF .

    ENDLOOP .

  ENDMETHOD .


  METHOD get_gos_attachment.

*    "variáveis locais
*    DATA: lt_contents_tab TYPE TABLE OF solix,
*          ls_document_str TYPE sofolenti1,
*          lv_xstrings_var TYPE xstring.
*
*    "limpar variável de retorno
*    REFRESH re_xstrings_tab.
*
*    "get the attachment through the business object using the Generic object services
*    DATA(ls_buobject_str) = VALUE sibflporb( typeid = im_objtypid_var
*                                             catid  = im_objcatid_var
*                                             instid = im_objinsid_var ).
*
*    TRY.
*        "read relationships of a fixed type
*        cl_binary_relation=>read_links_of_binrel( EXPORTING
*                                                    is_object   = ls_buobject_str
*                                                    ip_relation = cl_gos_api=>c_atta
*                                                    ip_role     = im_role_opt_var
*                                                  IMPORTING
*                                                    et_links    = DATA(lt_binlinks_tab) ).
*
*      CATCH cx_obl_parameter_error
*            cx_obl_internal_error
*            cx_obl_model_error.
*        "sair do processamento
*        RETURN.
*    ENDTRY.
*
*    LOOP AT lt_binlinks_tab ASSIGNING FIELD-SYMBOL(<fs_binlinks_str>).
*      CLEAR: ls_document_str, lv_xstrings_var.
*      REFRESH lt_contents_tab.
*      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
*        EXPORTING
*          document_id                = CONV so_entryid( <fs_binlinks_str>-instid_b )
*        IMPORTING
*          document_data              = ls_document_str
*        TABLES
*          contents_hex               = lt_contents_tab
*        EXCEPTIONS
*          document_id_not_exist      = 1
*          operation_no_authorization = 2
*          x_error                    = 3
*          OTHERS                     = 4.
*      IF sy-subrc IS NOT INITIAL.
*        "passar para o próximo registo
*        CONTINUE.
*      ENDIF.
*      "convert binary table to xstring
*      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*        EXPORTING
*          input_length = CONV i( ls_document_str-doc_size )
*        IMPORTING
*          buffer       = lv_xstrings_var
*        TABLES
*          binary_tab   = lt_contents_tab
*        EXCEPTIONS
*          failed       = 1
*          OTHERS       = 2.
*      IF sy-subrc IS NOT INITIAL.
*        "passar para o próximo registo
*        CONTINUE.
*      ENDIF.
*      APPEND lv_xstrings_var TO re_xstrings_tab.
*    ENDLOOP.

  ENDMETHOD.


  METHOD get_nast .

    TYPES:
      BEGIN OF ty_tipomensagem,
        tipomensagem TYPE zcds_nast-tipoMensagem,
      END OF ty_tipomensagem,
      range_tipomensagem TYPE RANGE OF ty_tipomensagem.

    CONSTANTS:
      lc_billing_doc  TYPE zcds_nast-tipoMensagem VALUE 'ZFA6',
      lc_outbound_del TYPE zcds_nast-tipoMensagem VALUE 'ZLD3'.

    CLEAR et_nast .

    DATA(lt_tipomensagem) =
      VALUE range_tipomensagem( LET s = rsmds_c_sign-including
                                    o = rsmds_c_option-equal
                                IN  sign   = s
                                    option = o
                                    ( low = lc_billing_doc )
                                    ( low = lc_outbound_del ) ) .

    IF ( lines( it_objky ) GT 0 ) .

      DATA(lt_objky) = it_objky .

      SORT lt_objky ASCENDING BY objky .
      DELETE ADJACENT DUPLICATES FROM lt_objky .
      DELETE lt_objky WHERE objky IS INITIAL .

      IF ( lines( lt_objky ) GT 0 ) .

        SELECT aplicacao, chaveobjeto, tipomensagem, idioma, partner,
               funcao, datacriacao, horacriacao, numarquivo, objecttype,
               iddocumento, tipodocumento, dispositivoSaida
          FROM zcds_nast
          INTO TABLE @et_nast
           FOR ALL ENTRIES IN @lt_objky
         WHERE chaveObjeto EQ @lt_objky-objky .

        IF ( sy-subrc EQ 0 ) AND
           ( lines( lt_tipomensagem ) GT 0 ) .
          DELETE et_nast WHERE tipomensagem NOT IN lt_tipomensagem .
        ENDIF .

      ENDIF .

    ENDIF .

  ENDMETHOD .

ENDCLASS.
