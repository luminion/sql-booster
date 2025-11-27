package com.example.mapper;

import com.example.entity.SysUser;
import com.example.vo.SysUserVO;
import io.github.luminion.sqlbooster.extension.mybatisplus.MyBatisPlusBoosterMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * 用户Mapper
 *
 * @author bootystar
 */
@Mapper
public interface SysUserMapper extends MyBatisPlusBoosterMapper<SysUser,SysUserVO> {


}
