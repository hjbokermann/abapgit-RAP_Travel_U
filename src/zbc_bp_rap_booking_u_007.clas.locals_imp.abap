CLASS lhc_Booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Booking.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Booking.

    METHODS read FOR READ
      IMPORTING keys FOR READ Booking RESULT result.

    METHODS rba_Travel FOR READ
      IMPORTING keys_rba FOR READ Booking\_Travel FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_Booking IMPLEMENTATION.

  METHOD delete.

    DATA lt_messages TYPE /dmo/t_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = <ls_key>-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = <ls_key>-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( booking_id = <ls_key>-bookingid ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( booking_id  = <ls_key>-bookingid
                                                    action_code = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages = lt_messages.

      IF lt_messages IS INITIAL.

        APPEND VALUE #( travelid = <ls_key>-travelid
                       bookingid = <ls_key>-bookingid ) TO mapped-booking.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = <ls_key>-travelid
                        bookingid = <ls_key>-bookingid ) TO failed-booking.

        LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = <ls_key>-travelid
                          bookingid = <ls_key>-bookingid
                  %msg = new_message( id = <ls_messages>-msgid
                                                number = <ls_messages>-msgno
                                                v1 = <ls_messages>-msgv1
                                                v2 = <ls_messages>-msgv2
                                                v3 = <ls_messages>-msgv3
                                                v4 = <ls_messages>-msgv4
                                                severity = CONV #( <ls_messages>-msgty ) )
         ) TO reported-booking.
        ENDLOOP.



      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update.

    DATA:
      lt_messages         TYPE /dmo/t_message,
      ls_legacy_entity_in TYPE /dmo/booking,
      ls_legacy_entity_x  TYPE /dmo/s_booking_inx.


    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entity>).

      ls_legacy_entity_in = CORRESPONDING #( <ls_entity> MAPPING FROM ENTITY ).

      ls_legacy_entity_x-booking_id = <ls_entity>-BookingID.
      ls_legacy_entity_x-_intx      = CORRESPONDING zbc_s_rap_booking_x_007( <ls_entity> MAPPING FROM ENTITY ).
      ls_legacy_entity_x-action_code = /dmo/if_flight_legacy=>action_code-update.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel      = VALUE /dmo/s_travel_in( travel_id = <ls_entity>-travelid )
          is_travelx     = VALUE /dmo/s_travel_inx( travel_id = <ls_entity>-travelid )
          it_booking     = VALUE /dmo/t_booking_in( ( CORRESPONDING #( ls_legacy_entity_in ) ) )
          it_bookingx    = VALUE /dmo/t_booking_inx( ( ls_legacy_entity_x ) )
        IMPORTING
          et_lt_messages = lt_messages.



      IF lt_messages IS INITIAL.

        APPEND VALUE #( travelid = <ls_entity>-travelid
                        bookingid = ls_legacy_entity_in-booking_id ) TO mapped-booking.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = <ls_entity>-travelid
                        bookingid = ls_legacy_entity_in-booking_id ) TO failed-booking.
        "fill reported structure to be displayed on the UI

        LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = <ls_entity>-travelid
                          bookingid = ls_legacy_entity_in-booking_id
                  %msg = new_message( id = <ls_messages>-msgid
                                                number = <ls_messages>-msgno
                                                v1 = <ls_messages>-msgv1
                                                v2 = <ls_messages>-msgv2
                                                v3 = <ls_messages>-msgv3
                                                v4 = <ls_messages>-msgv4
                                                severity = CONV #( <ls_messages>-msgty ) )
         ) TO reported-booking.
        ENDLOOP.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD read.

    DATA: ls_legacy_parent_entity_out TYPE /dmo/travel,
          ls_legacy_entities_out      TYPE /dmo/t_booking,
          lt_messages              TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key_parent>)
                            GROUP BY <ls_key_parent>-travelid .

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id   = <ls_key_parent>-travelid
        IMPORTING
          es_travel      = ls_legacy_parent_entity_out
          et_booking     = ls_legacy_entities_out
          et_lt_messages = lt_messages.

      IF lt_messages IS INITIAL.
        "For each travelID find the requested bookings
        LOOP AT GROUP <ls_key_parent> ASSIGNING FIELD-SYMBOL(<ls_key>)
                                       GROUP BY <ls_key>-%key.

          READ TABLE ls_legacy_entities_out INTO DATA(legacy_entity_out) WITH KEY travel_id  = <ls_key>-%key-TravelID
                                                                   booking_id = <ls_key>-%key-BookingID .
          "if read was successfull
          "fill result parameter with flagged fields
          IF sy-subrc = 0.

            INSERT CORRESPONDING #( legacy_entity_out MAPPING TO ENTITY ) INTO TABLE result.

          ELSE.
            "BookingID not found
            INSERT
              VALUE #( travelid    = <ls_key>-TravelID
                       bookingid   = <ls_key>-BookingID
                       %fail-cause = if_abap_behv=>cause-not_found )
              INTO TABLE failed-booking.
          ENDIF.
        ENDLOOP.
      ELSE.
        "TravelID not found or other fail cause
        LOOP AT GROUP <ls_key_parent> ASSIGNING <ls_key>.
          failed-booking = VALUE #(  BASE failed-booking
                                     FOR msg IN lt_messages ( %key-TravelID    = <ls_key>-TravelID
                                                             %key-BookingID   = <ls_key>-BookingID
                                                             %fail-cause      = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
                                                                                        THEN if_abap_behv=>cause-not_found
                                                                                        ELSE if_abap_behv=>cause-unspecific ) ) ).
        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD rba_Travel.

    DATA: ls_travel_out  TYPE /dmo/travel,
          lt_booking_out TYPE /dmo/t_booking,
          ls_travel      LIKE LINE OF result,
          lt_message     TYPE /dmo/t_message.

    "result  type table for read result /dmo/i_travel_u\\booking\_travel

    "Only one function call for each requested travelid
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<ls_travel>)
                                 GROUP BY <ls_travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <ls_travel>-%key-TravelID
        IMPORTING
          es_travel    = ls_travel_out
          et_messages  = lt_message.

      IF lt_message IS INITIAL.

        LOOP AT GROUP <ls_travel> ASSIGNING FIELD-SYMBOL(<ls_booking>).
          "fill link table with key fields
          INSERT VALUE #( source-%key = <ls_booking>-%key
                          target-%key = ls_travel_out-travel_id )
           INTO TABLE association_links .

          IF  result_requested  = abap_true.
            "fill result parameter with flagged fields
            ls_travel = CORRESPONDING #(  ls_travel_out MAPPING TO ENTITY ).
            INSERT ls_travel INTO TABLE result.
          ENDIF.
        ENDLOOP.

      ELSE. "fill failed table in case of error
        failed-booking = VALUE #(  BASE failed-booking
                              FOR msg IN lt_message ( %key-TravelID    = <ls_travel>-%key-TravelID
                                                      %key-BookingID   = <ls_travel>-%key-BookingID
                                                      %fail-cause      = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
                                                                                 THEN if_abap_behv=>cause-not_found
                                                                                ELSE if_abap_behv=>cause-unspecific ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
