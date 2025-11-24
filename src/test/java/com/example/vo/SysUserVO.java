package com.example.vo;

import com.example.entity.SysUser;
import com.example.mapper.SysUserMapper;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 用户-VO
 *
 * @author bootystar
 * @since 2025-07-25
 * @see SysUser
 * @see SysUserMapper
 */
@Data
public class SysUserVO {

    /**
     * id
     */
    private Long id;

    /**
     * 版本号
     */
    private Integer version;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    private LocalDateTime updateTime;

    /**
     * 姓名
     */
    private String name;
    
    private String nameLike;

    /**
     * 描述
     */
    private String description;

    /**
     * 年龄
     */
    private Integer age;

    /**
     * 生日
     */
    private LocalDate birthDate;

    /**
     * 状态
     */
    private Integer state;
}
