package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;
import io.github.luminion.sqlbooster.core.BoosterParam;

import java.util.List;

/**
 * 针对MyBatis-Plus 的 IService 扩展接口.
 * <p>
 * 集成了 {@link MybatisPlusBooster} 的能力,
 * 提供增强的数据库操作能力, 包括链式查询构建、Lambda 表达式支持等.
 *
 * @param <T> 实体类型
 * @param <V> VO(视图对象)类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterMpService<T, V> extends IService<T>, MybatisPlusBooster<T, V> {

    @Override
    default List<V> selectByBooster(BoosterParam<T> boosterParam, Object page) {
        BoosterMpMapper<T, V> baseMapper = (BoosterMpMapper<T, V>) getBaseMapper();
        return baseMapper.selectByBooster(boosterParam, page);
    }
}