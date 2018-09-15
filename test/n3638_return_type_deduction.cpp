#include <type_traits>

constexpr auto test1() {
    return 5;
}

constexpr decltype(auto) test1(int& a) {
    return a;
}

template<typename T>
constexpr auto test2() {
    if constexpr (sizeof(T) == sizeof(int)) {
        return char{};
    } else {
        return int{};
    }
}

#if CFTF_SUPPORT_ALIASING_FUNCTION_NAMES
// Same function name as the previous test2 but with a dummy parameter
template<typename T>
constexpr auto test2(int a) {
    if constexpr (sizeof(T) == sizeof(char)) {
        return char{};
    } else {
        return T{};
    }
}
#endif

template<typename T>
constexpr auto test3a(T& val) {
    return val;
}

template<typename T>
constexpr decltype(auto) test3b(T& val) {
    return val;
}

template<typename T>
constexpr decltype(auto) test3c(T val) {
    return val;
}

template<typename T>
constexpr decltype(auto) test3d(T val) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-stack-address"
    return (val);
#pragma clang diagnostic pop
}

// Trailing return type specifications shouldn't be matched by return type deduction
constexpr auto test4() -> char {
    return 5;
}

static_assert(std::is_same<decltype(test1()), int>::value);
int dummy;
static_assert(std::is_same<decltype(test1(dummy)), int&>::value);
static_assert(std::is_same<decltype(test2<char>()), int>::value);
static_assert(std::is_same<decltype(test2<int>()), char>::value);
#if CFTF_SUPPORT_ALIASING_FUNCTION_NAMES
static_assert(std::is_same<decltype(test2<char>(dummy)), char>::value);
static_assert(std::is_same<decltype(test2<int>(dummy)), int>::value);
#endif
static_assert(std::is_same<decltype(test3a(dummy)), int>::value);
static_assert(std::is_same<decltype(test3b(dummy)), int&>::value);
static_assert(std::is_same<decltype(test3c(dummy)), int>::value);
static_assert(std::is_same<decltype(test3d(dummy)), int&>::value);
static_assert(std::is_same<decltype(test4()), char>::value);
