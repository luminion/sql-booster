package io.github.luminion.sqlbooster.extension.pagehelper;

import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

public interface PhMapper<T, V> extends BoosterMapper<T, V>, PhBooster<T, V> {
}
