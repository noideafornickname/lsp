;;; 计算直线垂直方向的一个点
(defun vertical_point (
                       point ;;; 直线上一点
                       line_angle ;;; 直线弧度
                       clockwise ;;; 顺时针 nil=逆时针
                       dis ;;; 距离
                       )
  ;;; 计算度数
  (setq degress (* 180 (/ line_angle pi)))
  (if clockwise
    (setq degress (- degress 90))
    (setq degress (+ degress 90))
  )
  (setq line_v_angle (* pi (/ degress 180.0)))
  (polar point line_v_angle dis)
)



;;; 计算两条直线直接的相交矩形
;;;         /line1        \line2
;;;        /_______________\
;;;       /|______|________|\
;;;      /        p          \
;;;     /                     \
;;; 按顺时针方向先返回line1附近2个点(p1, p2)， 再返回line2附近2个点(p3, p4)
(defun rect_between_lines (
                           line1 ;;; 直线1
                           line2 ;;; 直线2
                           rect_line1_p1 ;;; 矩形上p2-p3, p4-p1上任意一点
                           rect_line1_angle ;;; 该点所在直线的弧度 <p2p3; <p4p1;
                           rect_thickness ;;; 矩形在垂直p所在直线方向上的厚度; 
                           ;;; 按顺时针方向 返回p1,p2,p3,p4个点时, rect_line_p1所在的线段
                           ;;; 2=p2,p3; 4=p4,p1
                           direct
                           )
  ;;; rect_line1角度值
  (setq rect_line1_angle_v (* 180 (/ rect_line1_angle pi)))
  ;;; 矩形line1随机取第二个点
  (setq rect_line1_p2 (polar rect_line1_p1 rect_line1_angle 100))
  ;;; 矩形line1平行的line2上取两个点
  (if (= direct 4)
    ;;; 垂直线的弧度
    (setq rect_line1_v_angle (* pi (/ (+ rect_line1_angle_v 90) 180)))
    (setq rect_line1_v_angle (* pi (/ (- rect_line1_angle_v 90) 180)))
  )
  (setq rect_line2_p1 (polar rect_line1_p1 rect_line1_v_angle rect_thickness))
  (setq rect_line2_p2 (polar rect_line2_p1 rect_line1_angle 100))
  
  ;;; 计算矩形两条边与两条直线的四个交点
  (setq line1_p1 (inters (nth 0 line1) (nth 1 line1) rect_line1_p1 rect_line1_p2 nil))
  (setq line1_p2 (inters (nth 0 line1) (nth 1 line1) rect_line2_p1 rect_line2_p2 nil))
  (setq line2_p1 (inters (nth 0 line2) (nth 1 line2) rect_line1_p1 rect_line1_p2 nil))
  (setq line2_p2 (inters (nth 0 line2) (nth 1 line2) rect_line2_p1 rect_line2_p2 nil))
  
  ;;; 调整为按line1,line2 的顺时针点位
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
  ;;; 从四个点中提取宽度最小的矩形
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

;;; 绘制多边形
(defun draw_ploygon (
                  points ;;;点位列表
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

;;; p2在p1右侧
(defun at_right (p1 p2)
  (> (nth 0 p2) (nth 0 p1))
)

;;; 绘制两条直线夹角圆弧 靠近圆心的直线上的坐标放前面
(defun draw_line_angle (
                        slp1 slp2 ;;; 开始直线坐标
                        elp1 elp2 ;;; 结束直线坐标
                        radius ;;; 半径
                        )
  (setq cross (inters slp1 slp2 elp1 elp2 nil)) ;;; 交点
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
                root_p   ;;; 钢箱梁顶板顶点坐标
                center_h ;;; 中心线顶板到底板的高度(包括钢板厚度)
                )
  (setq roof_pt 16) ;;; 顶板钢板厚度PlateThickness
  (setq floor_pt 16) ;;; 底板钢板厚度PlateThickness
  (setq bridge_w 26700) ;;; 桥面宽度
  (setq roof_l_sine 0.02) ;;; 顶板左斜面正切值
  (setq roof_r_sine 0.02) ;;; 顶板右斜面正切值
  (setq floor_l_sine 0.02) ;;; 底板左斜面正切值
  (setq floor_r_sine 0.02) ;;; 底板右斜面正切值
  (setq floor_l_h_offset 42) ;;; 底板右斜面相对边腹板水平缩进
  (setq floor_r_h_offset 42) ;;; 底板左斜面相对边腹板水平缩进
  (setq side_web_r_vw 11243) ;;; 右边腹板中心线与中心线平距
  (setq side_web_l_vw 11243) ;;; 左边腹板中心线与中心线平距
  (setq side_web_r_sine 4) ;;; 右边腹板水平正切值  垂直正切值=nil
  (setq side_web_l_sine -4) ;;; 左边腹板水平正切值   垂直正切值=nil
  (setq roof_beam_offset 300) ;;; 顶部横梁相对顶板平移距离
  (setq roof_beam_pt 16) ;;; 顶部横梁厚度
  (setq floor_beam_offset 300) ;;; 底部横梁相对底板平移距离
  (setq floor_beam_pt 16) ;;; 底部横梁厚度
  (setq side_web_pt 14) ;;; 边腹板厚度
  (setq middle_web_pt 14) ;;; 中腹板厚度
  (setq middle_webs_vw (list 2250 6750 )) ;;; 单边中腹板到中心线平局列表
  (setq wing_f_pt 12) ;;; 悬臂侧翼板厚度
  (setq wing_f_h 280) ;;; 悬臂侧翼板高度
  (setq wing_f_offset 0) ;;; 悬臂侧翼板缩进
  (setq wing_b_pt 12) ;;; 悬臂底板厚度
  (setq wing_b_i_h 715) ;;; 悬臂底板内测垂直净高
  (setq wing_b_out_v_offet 15) ;;; 悬臂底板外侧相对侧翼板缩进
  (setq angle_remark_radis 35) ;;; 角度标注半径
  (setq center_x (nth 0 root_p)) ;;; 中线线x值
  (setq half_w (/ bridge_w 2.0))
  
  (setq roof_ct (list (nth 0 root_p) (nth 1 root_p))) ;;; 顶板中上点位

  ;;; 顶板开始
  ;;; 右顶板弧度
  (setq roof_r_angle (angle '(0 0) (list 10000 (* 10000 roof_r_sine))))
  ;;; 顶板右侧矩形区域
  (setq roof_r_rect (rect_between_lines 
                      (list roof_ct (list center_x 0))
                      (list (list (+ center_x (/ bridge_w 2)) 0) (list (+ center_x (/ bridge_w 2)) 100))
                      roof_ct roof_r_angle roof_pt 2
                    )
  )
  ;;; 左顶板弧度
  (setq roof_l_angle (angle '(0 0) (list 10000 (* 10000 roof_l_sine))))
  ;;; 左顶板矩形区域
  (setq roof_l_rect (rect_between_lines 
                      (list (list (- center_x (/ bridge_w 2)) 0) (list (- center_x (/ bridge_w 2)) 100))
                      (list roof_ct (list center_x 0))
                      roof_ct roof_l_angle roof_pt 2
                    )
  )
  ;;; 重新计算顶板中线线与顶板底边交点
  (setq roof_cb (inters roof_ct (polar roof_ct (/ pi 2.0) 100) (nth 0 roof_r_rect) (nth 3 roof_r_rect) nil))
  ;;; 完整的顶板多边形
  (setq roof_rect (list 
                  (nth 0 roof_l_rect) (nth 1 roof_l_rect) roof_ct 
                  (nth 2 roof_r_rect) (nth 3 roof_r_rect) roof_cb
                  )
  )
  ;;; 绘制顶板区域
  (draw_ploygon roof_rect)
  ;;; 顶板结束
  (prin1 "顶板完成")
  ;;; 底板计算
  (setq floor_cb (list center_x (- (nth 1 (nth 2 roof_rect)) center_h))) ;;; 底板下边中心点
  (setq floor_r_angle (angle '(0 0) (list 1000 (* 1000 floor_r_sine)))) ;;; 右底板弧度
  (setq floor_l_angle (angle '(0 0) (list 1000 (* 1000 floor_l_sine)))) ;;; 左底板弧度
  (setq floor_r_t_p (vertical_point floor_cb floor_r_angle nil floor_pt)) ;;; 底板右上面某一点
  (setq floor_l_t_p (vertical_point floor_cb floor_l_angle nil floor_pt)) ;;; 底板左上面某一点
  (setq floor_ct (inters 
                   floor_cb (nth 2 roof_rect)
                   floor_r_t_p (polar floor_r_t_p floor_r_angle 100)
                   nil
                 )
  )
  
  ;;; 右边腹板开始
  
  ;;; 右边腹板角度
  (if side_web_r_sine
    (setq sw_r_angle (angle '(0 0) (list 1000 (* 1000 side_web_r_sine))))
    (setq sw_r_angle (/ pi 2.0))
  )
  ;;; 右边腹板中线线与顶板交点
  (setq sw_r_ct (inters (nth 5 roof_rect) (nth 4 roof_rect) (list (+ side_web_r_vw center_x) 0) (list (+ side_web_r_vw center_x) 100) nil))
  ;;; 右边腹板左上侧面上某一点
  (setq sw_r_p (vertical_point sw_r_ct sw_r_angle nil (/ side_web_pt 2)))
  ;;; 右边腹板矩形
  (setq sw_r_rect (rect_between_lines 
                    (list (nth 5 roof_rect) (nth 4 roof_rect)) ;;； 顶板底边
                    (list floor_ct floor_r_t_p) ;;; 底板顶边
                    sw_r_p (+ sw_r_angle pi) side_web_pt 2
                  ))
  
  (draw_ploygon sw_r_rect)
  ;;; 右边腹板结束
  (prin1 "右边腹板完成")
  
  ;;; 左边腹板开始
  
  ;;; 左边腹板角度
  (if side_web_l_sine
    (setq sw_l_angle (angle '(0 0) (list 1000 (* 1000 side_web_l_sine))))
    (setq sw_l_angle (/ pi 2.0))
  )
  
  ;;; 右边腹板中线线与顶板交点
  (setq sw_l_ct (inters (nth 0 roof_rect) (nth 5 roof_rect) (list (- center_x side_web_l_vw) 0) (list (- center_x side_web_l_vw) 100) nil))
  ;;; 左边腹板左侧面上某一点
  (setq sw_l_p (vertical_point sw_l_ct sw_l_angle nil (/ side_web_pt 2)))
  ;;; 左边腹板矩形
  (setq sw_l_rect (rect_between_lines 
                    (list (nth 0 roof_rect) (nth 5 roof_rect)) ;;； 顶板底边
                    (list floor_ct floor_l_t_p) ;;; 底板顶边
                    sw_l_p sw_l_angle side_web_pt 2
                  ))
  
  (draw_ploygon sw_l_rect)
  ;;; 左边腹板结束
  (prin1 "左边腹板完成")
  
  ;;; 底板开始
  ;;; 右边腹板中心线与底板交点
  (setq sw_r_cb (inters floor_ct floor_r_t_p sw_r_ct (polar sw_r_ct sw_r_angle 100) nil))
  
  ;;; 右底板矩形
  (setq floor_r_rect (rect_between_lines 
                     (list floor_ct floor_cb)
                     (list (list (+ (nth 0 sw_r_cb) floor_r_h_offset) 0) (list (+ (nth 0 sw_r_cb) floor_r_h_offset) 100)) ;;; 边腹板中心线与底板交点向右偏移后的垂直线
                     floor_ct floor_r_angle floor_pt 2
                     ))
  
  ;;; 左边腹板中心线与底板交点
  (setq sw_l_cb (inters floor_ct floor_l_t_p sw_l_ct (polar sw_l_ct sw_l_angle 100) nil))
  
  ;;; 左底板矩形
  (setq floor_l_rect (rect_between_lines
                     (list (list (- (nth 0 sw_l_cb) floor_l_h_offset) 0) (list (- (nth 0 sw_l_cb) floor_l_h_offset) 100)) ;;; 边腹板中心线与底板交点向左偏移后的垂直线
                     (list floor_cb floor_ct)
                     floor_ct floor_l_angle floor_pt 2
                     ))
  ;;; 底板矩形
  (setq floor_rect (list 
                   (nth 0 floor_l_rect) (nth 1 floor_l_rect) floor_ct 
                   (nth 2 floor_r_rect) (nth 3 floor_r_rect) floor_cb 
                   ))
  
  (draw_ploygon floor_rect)
  ;;; 底板结束
  (prin1 "底板完成")
  
  ;;; 腹板开始
  
  (setq half_m_pt (/ middle_web_pt 2)) ;;; 中腹板半厚度
  ;;; 左中腹板
  (setq left_mw_list (list)) ;;; 左侧腹板集合 从中心向边上方向
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
  
  ;;; 右中腹板
  (setq right_mw_list (list)) ;;; 左侧腹板集合 从中心向边上方向
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
  ;;; 腹板结束
  (prin1 "腹板完成")
  
  ;;; 横梁开始
  (setq left_roof_beam_list (list)) ;;; 左侧顶部横梁列表
  (setq left_floor_beam_list (list)) ;;; 左侧底部横梁列表
  (setq roof_l_beam_p (vertical_point roof_cb roof_l_angle "true" roof_beam_offset));;; 左侧顶横梁任意一点
  (setq floor_l_beam_p (vertical_point floor_ct floor_l_angle nil floor_beam_offset));;; 左侧底横梁任意一点
  (setq line2 (list roof_ct roof_cb))
  (foreach mv_rect left_mw_list 
    (setq roof_beam_rect (rect_between_lines 
                      (list (nth 1 mv_rect) (nth 2 mv_rect)) ;;; 左中腹板靠近中线的一条边
                      line2
                      roof_l_beam_p roof_l_angle roof_beam_pt 2
                    ))
    (setq left_roof_beam_list (append left_roof_beam_list (list roof_beam_rect)))
    (setq floor_beam_rect (rect_between_lines 
                      (list (nth 1 mv_rect) (nth 2 mv_rect)) ;;; 左中腹板靠近中线的一条边
                      line2
                      floor_l_beam_p floor_l_angle floor_beam_pt 2
                    ))
    (setq left_floor_beam_list (append left_floor_beam_list (list floor_beam_rect)))
    (setq line2 (list (nth 0 mv_rect) (nth 3 mv_rect)))  ;;;  左中腹板远离中线的一条边
  )
  
  (setq right_roof_beam_list (list)) ;;; 右侧顶部横梁列表
  (setq right_floor_beam_list (list)) ;;; 右侧底部横梁列表
  (setq roof_r_beam_p (vertical_point roof_cb roof_r_angle "true" roof_beam_offset));;; 右侧顶横梁任意一点
  (setq floor_r_beam_p (vertical_point floor_ct floor_r_angle nil floor_beam_offset));;; 右侧底横梁任意一点
  (setq line1 (list roof_ct roof_cb))
  (foreach mv_rect right_mw_list 
    (setq roof_beam_rect (rect_between_lines 
                      line1
                      (list (nth 0 mv_rect) (nth 3 mv_rect)) ;;; 右中腹板靠近中线的一条边
                      roof_r_beam_p roof_r_angle roof_beam_pt 2
                    ))
    (setq right_roof_beam_list (append right_roof_beam_list (list roof_beam_rect)))
    (setq floor_beam_rect (rect_between_lines 
                      line1
                      (list (nth 0 mv_rect) (nth 3 mv_rect)) ;;; 右中腹板靠近中线的一条边
                      floor_r_beam_p floor_r_angle floor_beam_pt 2
                    ))
    (setq right_floor_beam_list (append right_floor_beam_list (list floor_beam_rect)))
    (setq line1 (list (nth 1 mv_rect) (nth 2 mv_rect)))  ;;;  右中腹板远离中线的一条边
  )
  
  ;;; 中心顶部横梁
  (setq center_top_beam_rect (list 
                            (nth 0 (nth 0 left_roof_beam_list)) (nth 1 (nth 0 left_roof_beam_list))
                            (inters (nth 1 (nth 0 left_roof_beam_list)) (nth 1 (nth 1 left_roof_beam_list)) roof_cb roof_ct nil)
                            (nth 2 (nth 0 right_roof_beam_list)) (nth 3 (nth 0 right_roof_beam_list))
                            (inters (nth 0 (nth 0 left_roof_beam_list)) (nth 0 (nth 1 left_roof_beam_list)) roof_cb roof_ct nil)
                        ))
  ;;; 中心底部横梁
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
  ;;; 横梁结束
  (prin1 "横梁完成")
  
  ;;; 翼板开始
  ;;; 右翼板
  (setq wing_r_p (list (- (+ center_x (/ bridge_w 2)) wing_f_offset) 0)) ;;; 右翼板p2p3线上某一点
  ;;; 随机构造一个翼板矩形提取p1,p2
  (setq wing_r_rect (rect_between_lines 
                      (list (nth 5 roof_rect) (nth 4 roof_rect)) ;;; 顶部底边
                      (list (list 0 (- (nth 1 roof_cb) (* 10.0 center_h))) (list 100 (- (nth 1 roof_cb) (* 10.0 center_h)))) ;;; 在底部构造一个随机的水平线
                      wing_r_p (* 3 (/ pi 2)) wing_f_pt 2
                    ))
  (setq wing_r_rect (list
                    (nth 0 wing_r_rect) (nth 1 wing_r_rect)
                    (list (nth 0 (nth 1 wing_r_rect)) (- (nth 1 (nth 1 wing_r_rect)) wing_f_h)) (list (nth 0 (nth 0 wing_r_rect)) (- (nth 1 (nth 0 wing_r_rect)) wing_f_h))
                    ))
  (draw_ploygon wing_r_rect)
  ;;; 右翼底板
  ;;; 右边腹板中心线与顶板交点向下设定高度的水平线与边腹板的交点作为翼板底板的中线线与边腹板的交点
  (setq wing_r_floor_cl (inters (nth 1 sw_r_rect) (nth 2 sw_r_rect) (list 0 (- (nth 1 sw_r_ct) wing_b_i_h)) (list 100 (- (nth 1 sw_r_ct) wing_b_i_h)) nil))
  ;;; 右边腹板弧度
  (setq wing_r_floor_angle (angle wing_r_floor_cl
                           (list (nth 0 (nth 3 wing_r_rect)) (+ (nth 1 (nth 3 wing_r_rect)) wing_b_out_v_offet));;; 右边腹板中心线与侧翼板交点
                           ))
  (setq wing_r_floor_rect (rect_between_lines 
                          (list (nth 1 sw_r_rect) (nth 2 sw_r_rect)) ;;; 右边腹板右边
                          (list (nth 0 wing_r_rect) (nth 3 wing_r_rect))
                          (vertical_point wing_r_floor_cl wing_r_floor_angle nil (/ wing_b_pt 2))
                          wing_r_floor_angle wing_b_pt 2
                          ))
  (draw_ploygon wing_r_floor_rect)
  
  ;;; 左翼板
  (setq wing_l_p (list (+ (- center_x (/ bridge_w 2)) wing_f_offset) 0)) ;;; 左翼板p4p1线上某一点
  ;; 随机构造一个翼板矩形提取p1,p2
  (setq wing_l_rect (rect_between_lines 
                      (list (nth 0 roof_rect) (nth 5 roof_rect)) ;;; 顶部底边
                      (list (list 0 (- (nth 1 roof_cb) (* 10.0 center_h))) (list 100 (- (nth 1 roof_cb) (* 10.0 center_h)))) ;;; 在底部构造一个随机的水平线
                      wing_l_p (* 3 (/ pi 2)) wing_f_pt 4
                    ))
  (setq wing_l_rect (list
                    (nth 0 wing_l_rect) (nth 1 wing_l_rect)
                    (list (nth 0 (nth 1 wing_l_rect)) (- (nth 1 (nth 1 wing_l_rect)) wing_f_h)) (list (nth 0 (nth 0 wing_l_rect)) (- (nth 1 (nth 0 wing_l_rect)) wing_f_h))
                    ))
  (draw_ploygon wing_l_rect)
  ;;; 左翼底板
  ;;; 左边腹板中心线与顶板交点向下设定高度的水平线与边腹板的交点作为翼板底板的中线线与边腹板的交点
  (setq wing_l_floor_cl (inters (nth 3 sw_l_rect) (nth 0 sw_l_rect) (list 0 (- (nth 1 sw_l_ct) wing_b_i_h)) (list 100 (- (nth 1 sw_l_ct) wing_b_i_h)) nil))
  ;;; 左边腹板弧度
  (setq wing_l_floor_angle (angle
                           (list (nth 0 (nth 2 wing_l_rect)) (+ (nth 1 (nth 2 wing_l_rect)) wing_b_out_v_offet));;; 左边腹板中心线与侧翼板交点
                             wing_l_floor_cl
                           ))
  (setq wing_l_floor_rect (rect_between_lines 
                          (list (nth 1 wing_l_rect) (nth 2 wing_l_rect))
                          (list (nth 3 sw_l_rect) (nth 0 sw_l_rect)) ;;; 左边腹板右边
                          (vertical_point wing_l_floor_cl wing_l_floor_angle nil (/ wing_b_pt 2))
                          wing_l_floor_angle wing_b_pt 2
                          ))
  (draw_ploygon wing_l_floor_rect)
  ;;; 翼板结束
  (prin1 "翼板完成")
  
  ;;; 夹角标注
  ;;; 左翼板
  (draw_line_angle (nth 1 wing_l_rect) (nth 2 wing_l_rect) (nth 0 roof_rect) (nth 5 roof_rect) angle_remark_radis)
  (draw_line_angle (nth 1 wing_l_floor_rect) (nth 2 wing_l_floor_rect) (nth 2 wing_l_rect) (nth 1 wing_l_rect) angle_remark_radis)
  ;;; 右翼板
  (draw_line_angle (nth 4 roof_rect) (nth 5 roof_rect) (nth 0 wing_r_rect) (nth 3 wing_r_rect) angle_remark_radis)
  (draw_line_angle (nth 3 wing_r_rect) (nth 0 wing_r_rect) (nth 2 wing_r_floor_rect) (nth 1 wing_r_floor_rect) angle_remark_radis)
  ;;; 左边腹板
  (draw_line_angle (nth 3 sw_l_rect) (nth 0 sw_l_rect) (nth 2 wing_l_floor_rect) (nth 1 wing_l_floor_rect) angle_remark_radis)
  ;;; 右边腹板
  (draw_line_angle (nth 1 wing_r_floor_rect) (nth 2 wing_r_floor_rect) (nth 2 sw_r_rect) (nth 1 sw_r_rect) angle_remark_radis)
  ;;; 腹板与顶部底板夹角
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
  ;;; 横梁与腹板
  (setq i 0)
  (setq len (length left_roof_beam_list))
  (while (< i len)
    (setq l_r_b_rect (nth i left_roof_beam_list)) ;;; 左顶横梁
    (setq r_r_b_rect (nth i right_roof_beam_list)) ;;; 右顶横梁
    (setq l_f_b_rect (nth i left_floor_beam_list)) ;;; 左底横梁
    (setq r_f_b_rect (nth i right_floor_beam_list)) ;;; 右底横梁
    (setq l_mv_rect (nth i left_mw_list)) ;;; 左侧腹板
    (setq r_mv_rect (nth i right_mw_list)) ;;; 右侧腹板
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
  ;;; 夹角标注结束
  (prin1 "夹角标注完成")
  (prin1 "图像绘制完成")
)

(defun c:gxl (/)
  (setq #os1 (getvar "osmode"));;保存捕捉值
  (setvar "osmode" 0);;关闭捕捉
  
  (setq bridge_sections_h (list 2400)) ;;; 桥面分段高度列表
  (setq vertical_span 8000) ;;; 垂直分布间隔距离
  (setq root_p (getpoint "选择钢箱梁顶板顶点坐标:")) ;;; 钢箱梁顶板顶点坐标
  (setq root_x (nth 0 root_p))
  (setq root_y (nth 1 root_p))
  (foreach h bridge_sections_h
    (draw_gxl (list root_x root_y) h)
    (setq root_y (- root_y h vertical_span))
  )
  
  (setvar "osmode" #os1);;还原捕捉设置
)