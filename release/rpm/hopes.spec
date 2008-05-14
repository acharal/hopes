%define name    hopes
%define version 0.0.5
%define release 1

Name:           %{name}
Version:        %{version}
Release:        %{release}
License:        GPL
Group:          Development/Languages
URL:            http://www.ajn.awmn.net/hopes/
Source:         %{name}-%{version}.tar.gz
Packager:       Angelos Charalambidis <a.charalambidis@di.uoa.gr>
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
Prefix:         %{_prefix}
BuildRequires:  ghc
Summary:        Interpreter for Higher order prolog with extensional semantics

%description
Authors:
--------
    Angelos Charalambidis <a.charalambidis@di.uoa.gr>

%prep
%setup

%build
runhaskell Setup.lhs configure --prefix=%{_prefix} --docdir=%{_datadir}/doc/packages/%{name}
runhaskell Setup.lhs build
cd doc
test -f configure || autoreconf

%install
runhaskell Setup.lhs copy --destdir=${RPM_BUILD_ROOT}

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%doc LICENSE
%doc README
