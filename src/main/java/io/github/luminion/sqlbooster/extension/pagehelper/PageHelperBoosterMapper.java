package io.github.luminion.sqlbooster.extension.pagehelper;

import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

/**
 * @author luminion
 * @since 1.0.0
 */
public interface PageHelperBoosterMapper<T, V> extends BoosterMapper<T, V>, PageHelperBoosterSupport<T, V> {
}
