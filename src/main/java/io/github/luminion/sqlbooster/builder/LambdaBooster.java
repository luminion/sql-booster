package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

import java.util.List;
import java.util.Optional;

/**
 * 基于 Lambda 语法的增强查询执行链。
 * <p>
 * 该类继承了底层的动态 SQL 拼接能力，并融合了 {@link Booster} 的核心执行引擎。
 * 开发者可通过极其流畅的链式 API (Fluent API) 动态组合查询条件，
 * 并最终通过 {@code first()}, {@code list()}, {@code page()} 等终端操作直接触发数据库查询，
 * 自动将结果映射为指定的 VO 类型。
 * </p>
 *
 * <p>使用示例：</p>
 * <pre>{@code
 * List<SysUserVO> list = mapper.lambdaBooster()
 *     .eq(SysUser::getState, 1)
 *     .like(SysUser::getName, "mike")
 *     .list();
 * }</pre>
 *
 * @param <T> 数据库实体类型
 * @param <V> 查询结果的 VO 类型
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 */
@SuppressWarnings("unused")
public class LambdaBooster<T, V> extends LambdaSqlBuilder<T, LambdaBooster<T, V>> {
    private final Booster<T, V> booster;

    public LambdaBooster(Booster<T, V> booster) {
        super(GenericTypeUtils.resolveBoosterEntityClass(booster));
        this.booster = booster;
    }

    public LambdaBooster(Booster<T, V> booster, SqlContext<T> sqlContext) {
        super(GenericTypeUtils.resolveBoosterEntityClass(booster));
        this.booster = booster;
        this.sqlContext.merge(sqlContext);
    }

    @Override
    protected LambdaBooster<T, V> newInstance() {
        return new LambdaBooster<>(booster);
    }

    public V first() {
        return booster.voFirst(this.sqlContext);
    }

    public <R> R first(Class<R> targetType) {
        return booster.voFirst(this.sqlContext, targetType);
    }

    public Optional<V> firstOpt() {
        return booster.voFirstOpt(this.sqlContext);
    }

    public <R> Optional<R> firstOpt(Class<R> targetType) {
        return booster.voFirstOpt(this.sqlContext, targetType);
    }

    public V unique() {
        return booster.voUnique(this.sqlContext);
    }

    public <R> R unique(Class<R> targetType) {
        return booster.voUnique(this.sqlContext, targetType);
    }

    public Optional<V> uniqueOpt() {
        return booster.voUniqueOpt(this.sqlContext);
    }

    public <R> Optional<R> uniqueOpt(Class<R> targetType) {
        return booster.voUniqueOpt(this.sqlContext, targetType);
    }

    public List<V> list() {
        return booster.voList(this.sqlContext);
    }

    public <R> List<R> list(Class<R> targetType) {
        return booster.voList(this.sqlContext, targetType);
    }

    public BPage<V> page(int pageNum, int pageSize) {
        return booster.voPage(this.sqlContext, pageNum, pageSize);
    }

    public BPage<V> page(long pageNum, long pageSize) {
        return booster.voPage(this.sqlContext, pageNum, pageSize);
    }

    public <R> BPage<R> page(int pageNum, int pageSize, Class<R> targetType) {
        return booster.voPage(this.sqlContext, pageNum, pageSize, targetType);
    }

    public <R> BPage<R> page(long pageNum, long pageSize, Class<R> targetType) {
        return booster.voPage(this.sqlContext, pageNum, pageSize, targetType);
    }

}
