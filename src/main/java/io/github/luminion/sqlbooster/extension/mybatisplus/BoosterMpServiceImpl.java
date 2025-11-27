package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.sqlbooster.core.BoosterParam;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 ServiceImpl的扩展.
 *
 * @author luminion
 * @since 1.0.0
 */
public abstract class BoosterMpServiceImpl<M extends BoosterMpMapper<T, V>, T, V> extends ServiceImpl<M, T> implements BoosterMpService<T, V> {

    @Override
    public List<V> selectByBooster(BoosterParam<T> boosterParam, Object page) {
        return getBaseMapper().selectByBooster(boosterParam, page);
    }   

}
