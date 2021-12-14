;;; ����ֱ�ߴ�ֱ�����һ����
(defun vertical_point (
                       point ;;; ֱ����һ��
                       line_angle ;;; ֱ�߻���
                       clockwise ;;; ˳ʱ�� nil=��ʱ��
                       dis ;;; ����
                       )
  ;;; �������
  (setq degress (* 180 (/ line_angle pi)))
  (if clockwise
    (setq degress (- degress 90))
    (setq degress (+ degress 90))
  )
  (setq line_v_angle (* pi (/ degress 180.0)))
  (polar point line_v_angle dis)
)



;;; ��������ֱ��ֱ�ӵ��ཻ����
;;;         /line1        \line2
;;;        /_______________\
;;;       /|______|________|\
;;;      /        p          \
;;;     /                     \
;;; ��˳ʱ�뷽���ȷ���line1����2����(p1, p2)�� �ٷ���line2����2����(p3, p4)
(defun rect_between_lines (
                           line1 ;;; ֱ��1
                           line2 ;;; ֱ��2
                           rect_line1_p1 ;;; ������p2-p3, p4-p1������һ��
                           rect_line1_angle ;;; �õ�����ֱ�ߵĻ��� <p2p3; <p4p1;
                           rect_thickness ;;; �����ڴ�ֱp����ֱ�߷����ϵĺ��; 
                           ;;; ��˳ʱ�뷽�� ����p1,p2,p3,p4����ʱ, rect_line_p1���ڵ��߶�
                           ;;; 2=p2,p3; 4=p4,p1
                           direct
                           )
  ;;; rect_line1�Ƕ�ֵ
  (setq rect_line1_angle_v (* 180 (/ rect_line1_angle pi)))
  ;;; ����line1���ȡ�ڶ�����
  (setq rect_line1_p2 (polar rect_line1_p1 rect_line1_angle 100))
  ;;; ����line1ƽ�е�line2��ȡ������
  (if (= direct 4)
    ;;; ��ֱ�ߵĻ���
    (setq rect_line1_v_angle (* pi (/ (+ rect_line1_angle_v 90) 180)))
    (setq rect_line1_v_angle (* pi (/ (- rect_line1_angle_v 90) 180)))
  )
  (setq rect_line2_p1 (polar rect_line1_p1 rect_line1_v_angle rect_thickness))
  (setq rect_line2_p2 (polar rect_line2_p1 rect_line1_angle 100))
  
  ;;; �������������������ֱ�ߵ��ĸ�����
  (setq line1_p1 (inters (nth 0 line1) (nth 1 line1) rect_line1_p1 rect_line1_p2 nil))
  (setq line1_p2 (inters (nth 0 line1) (nth 1 line1) rect_line2_p1 rect_line2_p2 nil))
  (setq line2_p1 (inters (nth 0 line2) (nth 1 line2) rect_line1_p1 rect_line1_p2 nil))
  (setq line2_p2 (inters (nth 0 line2) (nth 1 line2) rect_line2_p1 rect_line2_p2 nil))
  
  ;;; ����Ϊ��line1,line2 ��˳ʱ���λ
  (if (= direct 2)
    (progn
      (setq p1 line1_p2)
      (setq p2 line1_p1)
      (setq p3 line2_p1)
      (setq p4 line2_p2) 
    )
  )
  (if (= direct 4)
    (progn
      (setq p1 line1_p1)
      (setq p2 line1_p2)
      (setq p3 line2_p2)
      (setq p4 line2_p1) 
    )
  )
  ;;; ���ĸ�������ȡ�����С�ľ���
  (setq w1 (distance p1 p4))
  (setq w2 (distance p1 (inters p1 p4 p3 (polar p3 rect_line1_v_angle rect_thickness ) nil)))
  (setq w3 (distance p2 p3))
  (setq w4 (distance p2 (inters p2 p3 p4 (polar p4 rect_line1_v_angle rect_thickness ) nil)))
  (setq min_w (min w1 (min w2 (min w3 w4))))
  
  (if (= min_w w1)
    (list p1 
          (inters p2 p3 p1 (polar p1 rect_line1_v_angle rect_thickness ) nil) 
          (inters p2 p3 p4 (polar p4 rect_line1_v_angle rect_thickness ) nil)
          p4
    )
    (if (= min_w w2)
      (list p1 
            (inters p2 p3 p1 (polar p1 rect_line1_v_angle rect_thickness ) nil) 
            p3      
            (inters p1 p4 p3 (polar p3 rect_line1_v_angle rect_thickness ) nil) 
      )
      (if (= min_w w3) 
        (list 
          (inters p1 p4 p2 (polar p2 rect_line1_v_angle rect_thickness ) nil)
          p2
          p3
          (inters p1 p4 p3 (polar p3 rect_line1_v_angle rect_thickness ) nil)
          
        )
        (list 
          (inters p1 p4 p2 (polar p2 rect_line1_v_angle rect_thickness ) nil)
          p2
          (inters p2 p3 p4 (polar p4 rect_line1_v_angle rect_thickness ) nil)
          p4
        )
      )
    )
  )
)

;;; ���ƶ����
(defun draw_ploygon (
                  points ;;;��λ�б�
                  / len i
                  )
  (command "line")
  (setq len (length points))
  (setq i 0)
  (while (< i len)
    (command (nth i points))
    (setq i (+ i 1))
  )
  (command "C")
)

;;; p2��p1�Ҳ�
(defun at_right (p1 p2)
  (> (nth 0 p2) (nth 0 p1))
)

;;; ��������ֱ�߼н�Բ�� ����Բ�ĵ�ֱ���ϵ������ǰ��
(defun draw_line_angle (
                        slp1 slp2 ;;; ��ʼֱ������
                        elp1 elp2 ;;; ����ֱ������
                        radius ;;; �뾶
                        )
  (setq cross (inters slp1 slp2 elp1 elp2 nil)) ;;; ����
  (setq start (polar cross (angle slp1 slp2) radius))
  (if (at_right slp1 slp2)
    (if (at_right start cross)
        (setq start (list (- (* 2 (nth 0 cross)) (nth 0 start)) (- (* 2 (nth 1 cross)) (nth 1 start))))
    )
    (if (at_right cross start)
        (setq start (list (- (* 2 (nth 0 cross)) (nth 0 start)) (- (* 2 (nth 1 cross)) (nth 1 start))))
     )
  )
  (setq end (polar cross (angle elp1 elp2) radius))
  (if (at_right elp1 elp2)
    (if (at_right end cross)
        (setq end (list (- (* 2 (nth 0 cross)) (nth 0 end)) (- (* 2 (nth 1 cross)) (nth 1 end))))
    )
    (if (at_right cross end)
        (setq end (list (- (* 2 (nth 0 cross)) (nth 0 end)) (- (* 2 (nth 1 cross)) (nth 1 end))))
     )
  )
  (command "arc" start "c" cross end)
)

;;;
(defun draw_gxl ( 
                root_p   ;;; ���������嶥������
                center_h ;;; �����߶��嵽�װ�ĸ߶�(�����ְ���)
                )
  (setq roof_pt 16) ;;; ����ְ���PlateThickness
  (setq floor_pt 16) ;;; �װ�ְ���PlateThickness
  (setq bridge_w 26700) ;;; ������
  (setq roof_l_sine 0.02) ;;; ������б������ֵ
  (setq roof_r_sine 0.02) ;;; ������б������ֵ
  (setq floor_l_sine 0.02) ;;; �װ���б������ֵ
  (setq floor_r_sine 0.02) ;;; �װ���б������ֵ
  (setq floor_l_h_offset 42) ;;; �װ���б����Ա߸���ˮƽ����
  (setq floor_r_h_offset 42) ;;; �װ���б����Ա߸���ˮƽ����
  (setq side_web_r_vw 11243) ;;; �ұ߸�����������������ƽ��
  (setq side_web_l_vw 11243) ;;; ��߸�����������������ƽ��
  (setq side_web_r_sine 4) ;;; �ұ߸���ˮƽ����ֵ  ��ֱ����ֵ=nil
  (setq side_web_l_sine -4) ;;; ��߸���ˮƽ����ֵ   ��ֱ����ֵ=nil
  (setq roof_beam_offset 300) ;;; ����������Զ���ƽ�ƾ���
  (setq roof_beam_pt 16) ;;; �����������
  (setq floor_beam_offset 300) ;;; �ײ�������Եװ�ƽ�ƾ���
  (setq floor_beam_pt 16) ;;; �ײ��������
  (setq side_web_pt 14) ;;; �߸�����
  (setq middle_web_pt 14) ;;; �и�����
  (setq middle_webs_vw (list 2250 6750 )) ;;; �����и��嵽������ƽ���б�
  (setq wing_f_pt 12) ;;; ���۲������
  (setq wing_f_h 280) ;;; ���۲����߶�
  (setq wing_f_offset 0) ;;; ���۲��������
  (setq wing_b_pt 12) ;;; ���۵װ���
  (setq wing_b_i_h 715) ;;; ���۵װ��ڲⴹֱ����
  (setq wing_b_out_v_offet 15) ;;; ���۵װ������Բ��������
  (setq angle_remark_radis 35) ;;; �Ƕȱ�ע�뾶
  (setq center_x (nth 0 root_p)) ;;; ������xֵ
  (setq half_w (/ bridge_w 2.0))
  
  (setq roof_ct (list (nth 0 root_p) (nth 1 root_p))) ;;; �������ϵ�λ

  ;;; ���忪ʼ
  ;;; �Ҷ��廡��
  (setq roof_r_angle (angle '(0 0) (list 10000 (* 10000 roof_r_sine))))
  ;;; �����Ҳ��������
  (setq roof_r_rect (rect_between_lines 
                      (list roof_ct (list center_x 0))
                      (list (list (+ center_x (/ bridge_w 2)) 0) (list (+ center_x (/ bridge_w 2)) 100))
                      roof_ct roof_r_angle roof_pt 2
                    )
  )
  ;;; �󶥰廡��
  (setq roof_l_angle (angle '(0 0) (list 10000 (* 10000 roof_l_sine))))
  ;;; �󶥰��������
  (setq roof_l_rect (rect_between_lines 
                      (list (list (- center_x (/ bridge_w 2)) 0) (list (- center_x (/ bridge_w 2)) 100))
                      (list roof_ct (list center_x 0))
                      roof_ct roof_l_angle roof_pt 2
                    )
  )
  ;;; ���¼��㶥���������붥��ױ߽���
  (setq roof_cb (inters roof_ct (polar roof_ct (/ pi 2.0) 100) (nth 0 roof_r_rect) (nth 3 roof_r_rect) nil))
  ;;; �����Ķ�������
  (setq roof_rect (list 
                  (nth 0 roof_l_rect) (nth 1 roof_l_rect) roof_ct 
                  (nth 2 roof_r_rect) (nth 3 roof_r_rect) roof_cb
                  )
  )
  ;;; ���ƶ�������
  (draw_ploygon roof_rect)
  ;;; �������
  (prin1 "�������")
  ;;; �װ����
  (setq floor_cb (list center_x (- (nth 1 (nth 2 roof_rect)) center_h))) ;;; �װ��±����ĵ�
  (setq floor_r_angle (angle '(0 0) (list 1000 (* 1000 floor_r_sine)))) ;;; �ҵװ廡��
  (setq floor_l_angle (angle '(0 0) (list 1000 (* 1000 floor_l_sine)))) ;;; ��װ廡��
  (setq floor_r_t_p (vertical_point floor_cb floor_r_angle nil floor_pt)) ;;; �װ�������ĳһ��
  (setq floor_l_t_p (vertical_point floor_cb floor_l_angle nil floor_pt)) ;;; �װ�������ĳһ��
  (setq floor_ct (inters 
                   floor_cb (nth 2 roof_rect)
                   floor_r_t_p (polar floor_r_t_p floor_r_angle 100)
                   nil
                 )
  )
  
  ;;; �ұ߸��忪ʼ
  
  ;;; �ұ߸���Ƕ�
  (if side_web_r_sine
    (setq sw_r_angle (angle '(0 0) (list 1000 (* 1000 side_web_r_sine))))
    (setq sw_r_angle (/ pi 2.0))
  )
  ;;; �ұ߸����������붥�彻��
  (setq sw_r_ct (inters (nth 5 roof_rect) (nth 4 roof_rect) (list (+ side_web_r_vw center_x) 0) (list (+ side_web_r_vw center_x) 100) nil))
  ;;; �ұ߸������ϲ�����ĳһ��
  (setq sw_r_p (vertical_point sw_r_ct sw_r_angle nil (/ side_web_pt 2)))
  ;;; �ұ߸������
  (setq sw_r_rect (rect_between_lines 
                    (list (nth 5 roof_rect) (nth 4 roof_rect)) ;;�� ����ױ�
                    (list floor_ct floor_r_t_p) ;;; �װ嶥��
                    sw_r_p (+ sw_r_angle pi) side_web_pt 2
                  ))
  
  (draw_ploygon sw_r_rect)
  ;;; �ұ߸������
  (prin1 "�ұ߸������")
  
  ;;; ��߸��忪ʼ
  
  ;;; ��߸���Ƕ�
  (if side_web_l_sine
    (setq sw_l_angle (angle '(0 0) (list 1000 (* 1000 side_web_l_sine))))
    (setq sw_l_angle (/ pi 2.0))
  )
  
  ;;; �ұ߸����������붥�彻��
  (setq sw_l_ct (inters (nth 0 roof_rect) (nth 5 roof_rect) (list (- center_x side_web_l_vw) 0) (list (- center_x side_web_l_vw) 100) nil))
  ;;; ��߸����������ĳһ��
  (setq sw_l_p (vertical_point sw_l_ct sw_l_angle nil (/ side_web_pt 2)))
  ;;; ��߸������
  (setq sw_l_rect (rect_between_lines 
                    (list (nth 0 roof_rect) (nth 5 roof_rect)) ;;�� ����ױ�
                    (list floor_ct floor_l_t_p) ;;; �װ嶥��
                    sw_l_p sw_l_angle side_web_pt 2
                  ))
  
  (draw_ploygon sw_l_rect)
  ;;; ��߸������
  (prin1 "��߸������")
  
  ;;; �װ忪ʼ
  ;;; �ұ߸�����������װ彻��
  (setq sw_r_cb (inters floor_ct floor_r_t_p sw_r_ct (polar sw_r_ct sw_r_angle 100) nil))
  
  ;;; �ҵװ����
  (setq floor_r_rect (rect_between_lines 
                     (list floor_ct floor_cb)
                     (list (list (+ (nth 0 sw_r_cb) floor_r_h_offset) 0) (list (+ (nth 0 sw_r_cb) floor_r_h_offset) 100)) ;;; �߸�����������װ彻������ƫ�ƺ�Ĵ�ֱ��
                     floor_ct floor_r_angle floor_pt 2
                     ))
  
  ;;; ��߸�����������װ彻��
  (setq sw_l_cb (inters floor_ct floor_l_t_p sw_l_ct (polar sw_l_ct sw_l_angle 100) nil))
  
  ;;; ��װ����
  (setq floor_l_rect (rect_between_lines
                     (list (list (- (nth 0 sw_l_cb) floor_l_h_offset) 0) (list (- (nth 0 sw_l_cb) floor_l_h_offset) 100)) ;;; �߸�����������װ彻������ƫ�ƺ�Ĵ�ֱ��
                     (list floor_cb floor_ct)
                     floor_ct floor_l_angle floor_pt 2
                     ))
  ;;; �װ����
  (setq floor_rect (list 
                   (nth 0 floor_l_rect) (nth 1 floor_l_rect) floor_ct 
                   (nth 2 floor_r_rect) (nth 3 floor_r_rect) floor_cb 
                   ))
  
  (draw_ploygon floor_rect)
  ;;; �װ����
  (prin1 "�װ����")
  
  ;;; ���忪ʼ
  
  (setq half_m_pt (/ middle_web_pt 2)) ;;; �и������
  ;;; ���и���
  (setq left_mw_list (list)) ;;; ��ู�弯�� ����������Ϸ���
  (foreach dis middle_webs_vw
    (setq mv_point (list (+ half_m_pt (- center_x dis)) 0))
    (setq mv_rect (rect_between_lines
                  (list (nth 0 roof_rect) (nth 5 roof_rect))
                  (list (nth 1 floor_rect) (nth 2 floor_rect))
                  mv_point (* 3 (/ pi 2)) middle_web_pt 2
                  ))
    (draw_ploygon mv_rect)
    (setq left_mw_list (append left_mw_list (list mv_rect)))
  )
  (setq left_mw_list (append left_mw_list (list sw_l_rect)))
  
  ;;; ���и���
  (setq right_mw_list (list)) ;;; ��ู�弯�� ����������Ϸ���
  (foreach dis middle_webs_vw
    (setq mv_point (list (+ half_m_pt (+ center_x dis)) 0))
    (setq mv_rect (rect_between_lines
                  (list (nth 5 roof_rect) (nth 4 roof_rect))
                  (list (nth 2 floor_rect) (nth 3 floor_rect))
                  mv_point (* 3 (/ pi 2)) middle_web_pt 2
                  ))
    (draw_ploygon mv_rect)
    (setq right_mw_list (append right_mw_list (list mv_rect)))
  )
  (setq right_mw_list (append right_mw_list (list sw_r_rect)))
  ;;; �������
  (prin1 "�������")
  
  ;;; ������ʼ
  (setq left_roof_beam_list (list)) ;;; ��ඥ�������б�
  (setq left_floor_beam_list (list)) ;;; ���ײ������б�
  (setq roof_l_beam_p (vertical_point roof_cb roof_l_angle "true" roof_beam_offset));;; ��ඥ��������һ��
  (setq floor_l_beam_p (vertical_point floor_ct floor_l_angle nil floor_beam_offset));;; ���׺�������һ��
  (setq line2 (list roof_ct roof_cb))
  (foreach mv_rect left_mw_list 
    (setq roof_beam_rect (rect_between_lines 
                      (list (nth 1 mv_rect) (nth 2 mv_rect)) ;;; ���и��忿�����ߵ�һ����
                      line2
                      roof_l_beam_p roof_l_angle roof_beam_pt 2
                    ))
    (setq left_roof_beam_list (append left_roof_beam_list (list roof_beam_rect)))
    (setq floor_beam_rect (rect_between_lines 
                      (list (nth 1 mv_rect) (nth 2 mv_rect)) ;;; ���и��忿�����ߵ�һ����
                      line2
                      floor_l_beam_p floor_l_angle floor_beam_pt 2
                    ))
    (setq left_floor_beam_list (append left_floor_beam_list (list floor_beam_rect)))
    (setq line2 (list (nth 0 mv_rect) (nth 3 mv_rect)))  ;;;  ���и���Զ�����ߵ�һ����
  )
  
  (setq right_roof_beam_list (list)) ;;; �Ҳඥ�������б�
  (setq right_floor_beam_list (list)) ;;; �Ҳ�ײ������б�
  (setq roof_r_beam_p (vertical_point roof_cb roof_r_angle "true" roof_beam_offset));;; �Ҳඥ��������һ��
  (setq floor_r_beam_p (vertical_point floor_ct floor_r_angle nil floor_beam_offset));;; �Ҳ�׺�������һ��
  (setq line1 (list roof_ct roof_cb))
  (foreach mv_rect right_mw_list 
    (setq roof_beam_rect (rect_between_lines 
                      line1
                      (list (nth 0 mv_rect) (nth 3 mv_rect)) ;;; ���и��忿�����ߵ�һ����
                      roof_r_beam_p roof_r_angle roof_beam_pt 2
                    ))
    (setq right_roof_beam_list (append right_roof_beam_list (list roof_beam_rect)))
    (setq floor_beam_rect (rect_between_lines 
                      line1
                      (list (nth 0 mv_rect) (nth 3 mv_rect)) ;;; ���и��忿�����ߵ�һ����
                      floor_r_beam_p floor_r_angle floor_beam_pt 2
                    ))
    (setq right_floor_beam_list (append right_floor_beam_list (list floor_beam_rect)))
    (setq line1 (list (nth 1 mv_rect) (nth 2 mv_rect)))  ;;;  ���и���Զ�����ߵ�һ����
  )
  
  ;;; ���Ķ�������
  (setq center_top_beam_rect (list 
                            (nth 0 (nth 0 left_roof_beam_list)) (nth 1 (nth 0 left_roof_beam_list))
                            (inters (nth 1 (nth 0 left_roof_beam_list)) (nth 1 (nth 1 left_roof_beam_list)) roof_cb roof_ct nil)
                            (nth 2 (nth 0 right_roof_beam_list)) (nth 3 (nth 0 right_roof_beam_list))
                            (inters (nth 0 (nth 0 left_roof_beam_list)) (nth 0 (nth 1 left_roof_beam_list)) roof_cb roof_ct nil)
                        ))
  ;;; ���ĵײ�����
  (setq center_bottom_bean_rect (list
                                (nth 0 (nth 0 left_floor_beam_list)) (nth 1 (nth 0 left_floor_beam_list))
                                (inters (nth 1 (nth 0 left_floor_beam_list)) (nth 1 (nth 1 left_floor_beam_list)) roof_cb roof_ct nil)
                                (nth 2 (nth 0 right_floor_beam_list)) (nth 3 (nth 0 right_floor_beam_list))
                                (inters (nth 0 (nth 0 left_floor_beam_list)) (nth 0 (nth 1 left_floor_beam_list)) roof_cb roof_ct nil)
                                ))
  (draw_ploygon center_top_beam_rect)
  (draw_ploygon center_bottom_bean_rect)
  (setq i 1)
  (setq len (length left_roof_beam_list))
  (while (< i len)
    (draw_ploygon (nth i left_roof_beam_list))
    (draw_ploygon (nth i right_roof_beam_list))
    (draw_ploygon (nth i left_floor_beam_list))
    (draw_ploygon (nth i right_floor_beam_list))
    (setq i (+ i 1))
    
  )
  ;;; ��������
  (prin1 "�������")
  
  ;;; ��忪ʼ
  ;;; �����
  (setq wing_r_p (list (- (+ center_x (/ bridge_w 2)) wing_f_offset) 0)) ;;; �����p2p3����ĳһ��
  ;;; �������һ����������ȡp1,p2
  (setq wing_r_rect (rect_between_lines 
                      (list (nth 5 roof_rect) (nth 4 roof_rect)) ;;; �����ױ�
                      (list (list 0 (- (nth 1 roof_cb) (* 10.0 center_h))) (list 100 (- (nth 1 roof_cb) (* 10.0 center_h)))) ;;; �ڵײ�����һ�������ˮƽ��
                      wing_r_p (* 3 (/ pi 2)) wing_f_pt 2
                    ))
  (setq wing_r_rect (list
                    (nth 0 wing_r_rect) (nth 1 wing_r_rect)
                    (list (nth 0 (nth 1 wing_r_rect)) (- (nth 1 (nth 1 wing_r_rect)) wing_f_h)) (list (nth 0 (nth 0 wing_r_rect)) (- (nth 1 (nth 0 wing_r_rect)) wing_f_h))
                    ))
  (draw_ploygon wing_r_rect)
  ;;; ����װ�
  ;;; �ұ߸����������붥�彻�������趨�߶ȵ�ˮƽ����߸���Ľ�����Ϊ���װ����������߸���Ľ���
  (setq wing_r_floor_cl (inters (nth 1 sw_r_rect) (nth 2 sw_r_rect) (list 0 (- (nth 1 sw_r_ct) wing_b_i_h)) (list 100 (- (nth 1 sw_r_ct) wing_b_i_h)) nil))
  ;;; �ұ߸��廡��
  (setq wing_r_floor_angle (angle wing_r_floor_cl
                           (list (nth 0 (nth 3 wing_r_rect)) (+ (nth 1 (nth 3 wing_r_rect)) wing_b_out_v_offet));;; �ұ߸��������������彻��
                           ))
  (setq wing_r_floor_rect (rect_between_lines 
                          (list (nth 1 sw_r_rect) (nth 2 sw_r_rect)) ;;; �ұ߸����ұ�
                          (list (nth 0 wing_r_rect) (nth 3 wing_r_rect))
                          (vertical_point wing_r_floor_cl wing_r_floor_angle nil (/ wing_b_pt 2))
                          wing_r_floor_angle wing_b_pt 2
                          ))
  (draw_ploygon wing_r_floor_rect)
  
  ;;; �����
  (setq wing_l_p (list (+ (- center_x (/ bridge_w 2)) wing_f_offset) 0)) ;;; �����p4p1����ĳһ��
  ;; �������һ����������ȡp1,p2
  (setq wing_l_rect (rect_between_lines 
                      (list (nth 0 roof_rect) (nth 5 roof_rect)) ;;; �����ױ�
                      (list (list 0 (- (nth 1 roof_cb) (* 10.0 center_h))) (list 100 (- (nth 1 roof_cb) (* 10.0 center_h)))) ;;; �ڵײ�����һ�������ˮƽ��
                      wing_l_p (* 3 (/ pi 2)) wing_f_pt 4
                    ))
  (setq wing_l_rect (list
                    (nth 0 wing_l_rect) (nth 1 wing_l_rect)
                    (list (nth 0 (nth 1 wing_l_rect)) (- (nth 1 (nth 1 wing_l_rect)) wing_f_h)) (list (nth 0 (nth 0 wing_l_rect)) (- (nth 1 (nth 0 wing_l_rect)) wing_f_h))
                    ))
  (draw_ploygon wing_l_rect)
  ;;; ����װ�
  ;;; ��߸����������붥�彻�������趨�߶ȵ�ˮƽ����߸���Ľ�����Ϊ���װ����������߸���Ľ���
  (setq wing_l_floor_cl (inters (nth 3 sw_l_rect) (nth 0 sw_l_rect) (list 0 (- (nth 1 sw_l_ct) wing_b_i_h)) (list 100 (- (nth 1 sw_l_ct) wing_b_i_h)) nil))
  ;;; ��߸��廡��
  (setq wing_l_floor_angle (angle
                           (list (nth 0 (nth 2 wing_l_rect)) (+ (nth 1 (nth 2 wing_l_rect)) wing_b_out_v_offet));;; ��߸��������������彻��
                             wing_l_floor_cl
                           ))
  (setq wing_l_floor_rect (rect_between_lines 
                          (list (nth 1 wing_l_rect) (nth 2 wing_l_rect))
                          (list (nth 3 sw_l_rect) (nth 0 sw_l_rect)) ;;; ��߸����ұ�
                          (vertical_point wing_l_floor_cl wing_l_floor_angle nil (/ wing_b_pt 2))
                          wing_l_floor_angle wing_b_pt 2
                          ))
  (draw_ploygon wing_l_floor_rect)
  ;;; ������
  (prin1 "������")
  
  ;;; �нǱ�ע
  ;;; �����
  (draw_line_angle (nth 1 wing_l_rect) (nth 2 wing_l_rect) (nth 0 roof_rect) (nth 5 roof_rect) angle_remark_radis)
  (draw_line_angle (nth 1 wing_l_floor_rect) (nth 2 wing_l_floor_rect) (nth 2 wing_l_rect) (nth 1 wing_l_rect) angle_remark_radis)
  ;;; �����
  (draw_line_angle (nth 4 roof_rect) (nth 5 roof_rect) (nth 0 wing_r_rect) (nth 3 wing_r_rect) angle_remark_radis)
  (draw_line_angle (nth 3 wing_r_rect) (nth 0 wing_r_rect) (nth 2 wing_r_floor_rect) (nth 1 wing_r_floor_rect) angle_remark_radis)
  ;;; ��߸���
  (draw_line_angle (nth 3 sw_l_rect) (nth 0 sw_l_rect) (nth 2 wing_l_floor_rect) (nth 1 wing_l_floor_rect) angle_remark_radis)
  ;;; �ұ߸���
  (draw_line_angle (nth 1 wing_r_floor_rect) (nth 2 wing_r_floor_rect) (nth 2 sw_r_rect) (nth 1 sw_r_rect) angle_remark_radis)
  ;;; �����붥���װ�н�
  (foreach rect left_mw_list
    (draw_line_angle (nth 5 roof_rect) (nth 0 roof_rect) (nth 0 rect) (nth 3 rect) angle_remark_radis)
    (draw_line_angle (nth 3 rect) (nth 0 rect) (nth 2 floor_rect) (nth 1 floor_rect) angle_remark_radis)
    (draw_line_angle (nth 1 floor_rect) (nth 2 floor_rect) (nth 2 rect) (nth 1 rect) angle_remark_radis)
    (draw_line_angle (nth 1 rect) (nth 2 rect) (nth 0 roof_rect) (nth 5 roof_rect) angle_remark_radis)
  )
  (foreach rect right_mw_list
    (draw_line_angle (nth 4 roof_rect) (nth 5 roof_rect) (nth 0 rect) (nth 3 rect) angle_remark_radis)
    (draw_line_angle (nth 3 rect) (nth 0 rect) (nth 3 floor_rect) (nth 2 floor_rect) angle_remark_radis)
    (draw_line_angle (nth 2 floor_rect) (nth 3 floor_rect) (nth 2 rect) (nth 1 rect) angle_remark_radis)
    (draw_line_angle (nth 1 rect) (nth 2 rect) (nth 5 roof_rect) (nth 4 roof_rect) angle_remark_radis)
  )
  ;;; �����븹��
  (setq i 0)
  (setq len (length left_roof_beam_list))
  (while (< i len)
    (setq l_r_b_rect (nth i left_roof_beam_list)) ;;; �󶥺���
    (setq r_r_b_rect (nth i right_roof_beam_list)) ;;; �Ҷ�����
    (setq l_f_b_rect (nth i left_floor_beam_list)) ;;; ��׺���
    (setq r_f_b_rect (nth i right_floor_beam_list)) ;;; �ҵ׺���
    (setq l_mv_rect (nth i left_mw_list)) ;;; ��ู��
    (setq r_mv_rect (nth i right_mw_list)) ;;; �Ҳู��
    (if (= i 0)
      (progn
        (draw_line_angle (nth 1 l_mv_rect) (nth 2 l_mv_rect) (nth 0 l_r_b_rect) (nth 3 l_r_b_rect) angle_remark_radis)
        (draw_line_angle (nth 1 l_r_b_rect) (nth 2 l_r_b_rect) (nth 2 l_mv_rect) (nth 1 l_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 1 l_mv_rect) (nth 2 l_mv_rect) (nth 0 l_f_b_rect) (nth 3 l_f_b_rect) angle_remark_radis)
        (draw_line_angle (nth 1 l_f_b_rect) (nth 2 l_f_b_rect) (nth 2 l_mv_rect) (nth 1 l_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 3 r_mv_rect) (nth 0 r_mv_rect) (nth 2 r_r_b_rect) (nth 1 r_r_b_rect) angle_remark_radis)
        (draw_line_angle (nth 3 r_r_b_rect) (nth 0 r_r_b_rect) (nth 0 r_mv_rect) (nth 3 r_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 3 r_mv_rect) (nth 0 r_mv_rect) (nth 2 r_f_b_rect) (nth 1 r_f_b_rect) angle_remark_radis)
        (draw_line_angle (nth 3 r_f_b_rect) (nth 0 r_f_b_rect) (nth 0 r_mv_rect) (nth 3 r_mv_rect) angle_remark_radis)
      )
      (progn
        (setq pre_l_mv_rect (nth (- i 1) left_mw_list))
        (setq pre_r_mv_rect (nth (- i 1) right_mw_list))
        
        (draw_line_angle (nth 1 l_r_b_rect) (nth 2 l_r_b_rect) (nth 2 l_mv_rect) (nth 1 l_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 1 l_mv_rect) (nth 2 l_mv_rect) (nth 0 l_r_b_rect) (nth 3 l_r_b_rect) angle_remark_radis)
        
        (draw_line_angle (nth 3 l_r_b_rect) (nth 0 l_r_b_rect) (nth 0 pre_l_mv_rect) (nth 3 pre_l_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 3 pre_l_mv_rect) (nth 0 pre_l_mv_rect) (nth 2 l_r_b_rect) (nth 1 l_r_b_rect) angle_remark_radis)
        
        (draw_line_angle (nth 1 l_f_b_rect) (nth 2 l_f_b_rect) (nth 2 l_mv_rect) (nth 1 l_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 1 l_mv_rect) (nth 2 l_mv_rect) (nth 0 l_f_b_rect) (nth 3 l_f_b_rect) angle_remark_radis)
        
        (draw_line_angle (nth 3 l_f_b_rect) (nth 0 l_f_b_rect) (nth 0 pre_l_mv_rect) (nth 3 pre_l_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 3 pre_l_mv_rect) (nth 0 pre_l_mv_rect) (nth 2 l_f_b_rect) (nth 1 l_f_b_rect) angle_remark_radis)
        
        
        (draw_line_angle (nth 1 r_r_b_rect) (nth 2 r_r_b_rect) (nth 2 pre_r_mv_rect) (nth 1 pre_r_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 1 pre_r_mv_rect) (nth 2 pre_r_mv_rect) (nth 0 r_r_b_rect) (nth 3 r_r_b_rect) angle_remark_radis)
        
        (draw_line_angle (nth 3 r_r_b_rect) (nth 0 r_r_b_rect) (nth 0 r_mv_rect) (nth 3 r_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 3 r_mv_rect) (nth 0 r_mv_rect) (nth 2 r_r_b_rect) (nth 1 r_r_b_rect) angle_remark_radis)
        
        (draw_line_angle (nth 1 r_f_b_rect) (nth 2 r_f_b_rect) (nth 2 pre_r_mv_rect) (nth 1 pre_r_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 1 pre_r_mv_rect) (nth 2 pre_r_mv_rect) (nth 0 r_f_b_rect) (nth 3 r_f_b_rect) angle_remark_radis)
        
        (draw_line_angle (nth 3 r_f_b_rect) (nth 0 r_f_b_rect) (nth 0 r_mv_rect) (nth 3 r_mv_rect) angle_remark_radis)
        (draw_line_angle (nth 3 r_mv_rect) (nth 0 r_mv_rect) (nth 2 r_f_b_rect) (nth 1 r_f_b_rect) angle_remark_radis)
      )
    )
    (setq i (+ i 1))
  )  
  ;;; �нǱ�ע����
  (prin1 "�нǱ�ע���")
  (prin1 "ͼ��������")
)

(defun c:gxl (/)
  (setq #os1 (getvar "osmode"));;���沶׽ֵ
  (setvar "osmode" 0);;�رղ�׽
  
  (setq bridge_sections_h (list 2400)) ;;; ����ֶθ߶��б�
  (setq vertical_span 8000) ;;; ��ֱ�ֲ��������
  (setq root_p (getpoint "ѡ����������嶥������:")) ;;; ���������嶥������
  (setq root_x (nth 0 root_p))
  (setq root_y (nth 1 root_p))
  (foreach h bridge_sections_h
    (draw_gxl (list root_x root_y) h)
    (setq root_y (- root_y h vertical_span))
  )
  
  (setvar "osmode" #os1);;��ԭ��׽����
)