package com.example.impl;

import com.baomidou.dynamic.datasource.annotation.DS;
import com.example.entity.SysUser;
import com.example.mapper.SysUserMapper;
import com.example.vo.SysUserVO;
import io.github.luminion.sqlbooster.extension.mybatisplus.MyBatisPlusBoosterServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 用户服务实现类
 *
 * @author bootystar
 */
@Service
@DS("mysql")
public class MysqlService extends MyBatisPlusBoosterServiceImpl<SysUserMapper, SysUser, SysUserVO> {

    //@Override
    //public List<SysUserVO> selectByBooster(BoosterParam<SysUser> params, IPage<?> page) {
    //    return getBaseMapper().selectByBooster(params, page);
    //}
}