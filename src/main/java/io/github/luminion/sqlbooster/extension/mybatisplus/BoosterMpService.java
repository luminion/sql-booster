package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 针对MyBatis-Plus 的 IService 扩展接口.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterMpService<T, V> extends IService<T>, BoosterMpEngine<T, V> {

    //@Override
    //default List<V> selectByBooster(QueryParam<T> boosterParam, Object page) {
    //    BoosterMpMapper<T, V> baseMapper = (BoosterMpMapper<T, V>) getBaseMapper();
    //    return baseMapper.selectByBooster(boosterParam, page);
    //}
}