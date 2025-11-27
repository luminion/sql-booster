package com.example.impl;

import com.baomidou.dynamic.datasource.annotation.DS;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.entity.SysUser;
import com.example.mapper.SysUserMapper;
import com.example.vo.SysUserVO;
import io.github.luminion.sqlbooster.core.BoosterParam;
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 用户服务实现类
 *
 * @author bootystar
 */
@Service
@DS("mysql")
public class MysqlService extends BoosterMpServiceImpl<SysUserMapper, SysUser, SysUserVO> {

    //@Override
    //public List<SysUserVO> selectByBooster(BoosterParam<SysUser> params, IPage<?> page) {
    //    return getBaseMapper().selectByBooster(params, page);
    //}
}