package io.github.luminion.sqlbooster.extension.pagehelper;

import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

/**
 * 基于 PageHelper 的 Mapper 接口，
 * <p>
 * 基于PageHelper的分页的BoosterMapper。
 */
public interface PhMapper<T, V> extends BoosterMapper<T, V>, PhBooster<T, V> {
}
