package com.example.impl;

import com.baomidou.dynamic.datasource.annotation.DS;
import com.example.entity.SysUser;
import com.example.mapper.SysUserMapper;
import com.example.vo.SysUserVO;
import io.github.luminion.sqlbooster.extension.pagehelper.PageHelperBoosterSupport;
import io.github.luminion.sqlbooster.model.BoosterParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 用户服务实现类
 *
 * @author bootystar
 */
@Service
@DS("postgresql")
public class PostgresService implements PageHelperBoosterSupport<SysUser,SysUserVO> {
    @Autowired
    private SysUserMapper sysUserMapper;


    @Override
    public List<SysUserVO> selectByBooster(BoosterParam<SysUser> boosterParam, Object page) {
        return sysUserMapper.selectByBooster(boosterParam,null);
    }
}
