package io.github.luminion.sqlbooster.extension.pagehelper;

import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

/**
 * @author luminion
 * @since 1.0.0
 */
public interface PhMapper<T, V> extends BoosterMapper<T, V>, PhBooster<T, V> {
}
